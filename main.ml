open Printf
open Helper
open Lexer
open Parser

let filter_after_map f lst =
  List.map (function Some x -> x | None -> failwith "invalid op")
  @@ List.filter (function Some x -> true | None -> false)
  @@ List.map f lst

let rec list_unique lst =
  let set = Hashtbl.create @@ List.length lst in
  let rec aux res = function
    | [] -> res
    | x :: xs ->
        if Hashtbl.mem set x then aux res xs
        else (
          Hashtbl.add set x () ;
          aux (x :: res) xs )
  in
  aux [] lst

let hashmap_of_list src =
  let hashmap = ref Hashmap.empty in
  List.iter (fun (k, v) -> hashmap := Hashmap.add k v !hashmap) src ;
  !hashmap

let integrate od nw = Hashmap.union (fun _ _ r -> Some r) od nw

let appfmt buf fmt =
  ksprintf (fun str -> Buffer.add_string buf (str ^ "\n")) fmt

let appstr buf str = Buffer.add_string buf (str ^ "\n")

let escape_string str =
  let buf = Buffer.create (String.length str) in
  let rec aux i =
    if i < String.length str then (
      ( match str.[i] with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | ch -> Buffer.add_char buf ch ) ;
      aux (i + 1) )
  in
  aux 0 ; Buffer.contents buf

let make_label () = make_id ".L"

type environment =
  { symbols: (string, ast) Hashmap.t
  ; parent: environment option
  ; freevars: (string * string) list ref }

let add_symbols_in_pattern symbols ptn =
  integrate symbols @@ hashmap_of_list
  @@ List.map (fun n -> (n, Var (make_id n)))
  @@ varnames_in_pattern ptn

let add_symbols_in_patterns symbols ptns =
  integrate symbols @@ hashmap_of_list
  @@ List.map (fun n -> (n, Var (make_id n)))
  @@ List.flatten
  @@ List.map varnames_in_pattern ptns

type type_toplevel =
  { mutable letfuncs: ast list
  ; mutable strings: ast list
  ; mutable typedefs: typedef list
  ; mutable exps_list: string list
  ; ctors_type: (string, string) Hashtbl.t
  ; exps: (string, string) Hashtbl.t
  ; records: (string, string) Hashtbl.t
  ; records_fields: (string, string list) Hashtbl.t
  ; mutable modulename: string list
  ; (* TODO: opened_modulename should be in type environment
   * rather than type type_toplevel, because
   * functions, exceptions, types, and etc. in the opened module
   * mask previously defined ones with the same names.
   * For example, the current implementation doesn't allow the following code:
   *     module ABC = struct let f () = 5 end ;;
   *     let f () = 3 ;;
   *     open ABC;;
   *     test (f ()) 5 ;; (* expect 5 but will get 3 *)
   *)
    mutable opened_modulename: string list
  ; mutable modules: (string, string) Hashtbl.t }

(* Used in analysis of LetAnd *)
exception Should_be_closure

exception LetDef of ast list * environment

let analyze asts =
  let toplevel =
    { letfuncs= []
    ; strings= []
    ; typedefs= []
    ; exps_list= []
    ; ctors_type= Hashtbl.create 16
    ; exps= Hashtbl.create 16
    ; records= Hashtbl.create 16
    ; records_fields= Hashtbl.create 16
    ; modulename= []
    ; opened_modulename= ["Stdlib."]
    ; modules= Hashtbl.create 16 }
  in
  let with_modulename name =
    String.concat "." @@ List.rev @@ (name :: toplevel.modulename)
  in
  let exprs2expr = function
    | [] -> Nope
    | [expr] -> expr
    | exprs -> ExprSeq exprs
  in
  let get_modulename_prefix modulename =
    let buf = Buffer.create 128 in
    List.iter (fun modname ->
        Buffer.add_string buf modname ;
        Buffer.add_char buf '.' )
    @@ List.rev @@ modulename ;
    Buffer.contents buf
  in
  let find_with_modulename find name =
    try ("", find name) with Not_found -> (
      try
        let rec aux modulename =
          let prefix = get_modulename_prefix modulename in
          try (prefix, find (prefix ^ name)) with Not_found -> (
            match modulename with _ :: xs -> aux xs | [] -> raise Not_found )
        in
        aux toplevel.modulename
      with Not_found ->
        let rec aux = function
          | prefix :: opened_modulename -> (
            try (prefix, find @@ prefix ^ name) with Not_found ->
              aux opened_modulename )
          | [] -> raise Not_found
        in
        aux toplevel.opened_modulename )
  in
  let hashtbl_find_with_modulename hashtbl name =
    find_with_modulename (fun x -> Hashtbl.find hashtbl x) name
  in
  let hashmap_find_with_modulename name hashmap =
    let _, res = find_with_modulename (fun x -> Hashmap.find x hashmap) name in
    res
  in
  let find_symbol env name =
    let rec aux depth env =
      try (depth, hashmap_find_with_modulename name env.symbols)
      with Not_found -> (
        match env.parent with
        | Some parent -> aux (depth + 1) parent
        | None ->
            failwith (sprintf "not found in analysis (find_symbol): %s" name) )
    in
    aux 0 env
  in
  let rec aux_ptn env ptn =
    match ptn with
    | IntValue _ | CharValue _ | UnitValue | EmptyList | PtnRange _ -> ptn
    | StringValue _ ->
        toplevel.strings <- ptn :: toplevel.strings ;
        ptn
    | TupleValue values ->
        TupleValue (List.map (fun x -> aux_ptn env x) values)
    | Cons (car, cdr) -> Cons (aux_ptn env car, aux_ptn env cdr)
    | Var name -> (
      match find_symbol env name with
      | 0, sym -> sym
      | _ -> failwith "[FATAL] variable not found in pattern analysis" )
    | PtnAlias (ptn, (Var _ as var)) ->
        PtnAlias (aux_ptn env ptn, aux_ptn env var)
    | PtnOr (lhs, rhs) -> PtnOr (aux_ptn env lhs, aux_ptn env rhs)
    | CtorApp (None, ctorname, arg) ->
        let arg =
          match arg with Some arg -> Some (aux_ptn env arg) | _ -> None
        in
        let name_prefix, typename =
          try hashtbl_find_with_modulename toplevel.ctors_type ctorname
          with Not_found -> hashtbl_find_with_modulename toplevel.exps ctorname
        in
        CtorApp (Some typename, name_prefix ^ ctorname, arg)
    | _ -> failwith "unexpected pattern"
  in
  let rec analyze_pattern_match_cases env cases =
    List.map
      (fun (ptn, whn, ast) ->
        let env' =
          {env with symbols= add_symbols_in_pattern env.symbols ptn}
        in
        ( aux_ptn env' ptn
        , (match whn with Some expr -> Some (aux env' expr) | None -> None)
        , aux env' ast ) )
      cases
  and aux env ast =
    match ast with
    | IntValue _ | CharValue _ | UnitValue | EmptyList -> ast
    | StringValue _ ->
        toplevel.strings <- ast :: toplevel.strings ;
        ast
    | TupleValue values -> TupleValue (List.map (fun x -> aux env x) values)
    | ArrayValue values -> ArrayValue (List.map (fun x -> aux env x) values)
    | RecordValue (None, fields) ->
        let key_fieldname, _ = List.hd fields in
        let name_prefix, typename =
          hashtbl_find_with_modulename toplevel.records key_fieldname
        in
        RecordValue
          ( Some typename
          , List.map
              (fun (name, ast) -> (name_prefix ^ name, aux env ast))
              fields )
    | RecordValueWith (base, fields) ->
        let key_fieldname, _ = List.hd fields in
        let name_prefix, typename =
          hashtbl_find_with_modulename toplevel.records key_fieldname
        in
        let fieldnames = Hashtbl.find toplevel.records_fields typename in
        let fields =
          hashmap_of_list
          @@ List.map
               (fun (fieldname, v) -> (name_prefix ^ fieldname, v))
               fields
        in
        let new_base = Var (make_id "var") in
        aux env
        @@ LetAnd
             ( false
             , [([new_base], base)]
             , Some
                 (RecordValue
                    ( None
                    , List.map
                        (fun fieldname ->
                          try (fieldname, Hashmap.find fieldname fields)
                          with Not_found ->
                            ( fieldname
                            , RecordDotAccess (None, new_base, fieldname) ) )
                        fieldnames )) )
    | RecordDotAccess (None, ast, fieldname) ->
        let name_prefix, typename =
          hashtbl_find_with_modulename toplevel.records fieldname
        in
        RecordDotAccess (Some typename, aux env ast, name_prefix ^ fieldname)
    | Cons (car, cdr) -> Cons (aux env car, aux env cdr)
    | Add (lhs, rhs) -> Add (aux env lhs, aux env rhs)
    | Sub (lhs, rhs) -> Sub (aux env lhs, aux env rhs)
    | Mul (lhs, rhs) -> Mul (aux env lhs, aux env rhs)
    | Div (lhs, rhs) -> Div (aux env lhs, aux env rhs)
    | Rem (lhs, rhs) -> Rem (aux env lhs, aux env rhs)
    | LogicalLeftShift (lhs, rhs) -> LogicalLeftShift (aux env lhs, aux env rhs)
    | LogicalRightShift (lhs, rhs) ->
        LogicalRightShift (aux env lhs, aux env rhs)
    | ArithmeticRightShift (lhs, rhs) ->
        ArithmeticRightShift (aux env lhs, aux env rhs)
    | BitwiseAnd (lhs, rhs) -> BitwiseAnd (aux env lhs, aux env rhs)
    | BitwiseOr (lhs, rhs) -> BitwiseOr (aux env lhs, aux env rhs)
    | StringConcat (lhs, rhs) -> StringConcat (aux env lhs, aux env rhs)
    | ListConcat (lhs, rhs) -> ListConcat (aux env lhs, aux env rhs)
    | RefAssign (lhs, rhs) -> RefAssign (aux env lhs, aux env rhs)
    | RecordAssign (None, lhs, fieldname, rhs) ->
        let name_prefix, typename =
          hashtbl_find_with_modulename toplevel.records fieldname
        in
        RecordAssign
          (Some typename, aux env lhs, name_prefix ^ fieldname, aux env rhs)
    | Deref ast -> Deref (aux env ast)
    | Negate ast -> Negate (aux env ast)
    | Positate ast -> Positate (aux env ast)
    | StructEqual (lhs, rhs) -> StructEqual (aux env lhs, aux env rhs)
    | StructInequal (lhs, rhs) -> StructInequal (aux env lhs, aux env rhs)
    | LessThan (lhs, rhs) -> LessThan (aux env lhs, aux env rhs)
    | LessThanEqual (lhs, rhs) -> LessThanEqual (aux env lhs, aux env rhs)
    | LogicalAnd (lhs, rhs) -> LogicalAnd (aux env lhs, aux env rhs)
    | LogicalOr (lhs, rhs) -> LogicalOr (aux env lhs, aux env rhs)
    | IfThenElse (cond, then_body, Some else_body) ->
        IfThenElse (aux env cond, aux env then_body, Some (aux env else_body))
    | IfThenElse (cond, then_body, None) ->
        IfThenElse (aux env cond, aux env then_body, None)
    | ExprSeq exprs -> ExprSeq (List.map (fun x -> aux env x) exprs)
    | Lambda (args, body) ->
        let funcname = ".lambda" in
        aux env
        @@ LetAnd (false, [(Var funcname :: args, body)], Some (Var funcname))
    | StringGet (str, idx) ->
        (* a.[b] returns a b-th character of a string a.
         * Therefore, convert it to String.get call *)
        aux env @@ AppCls (Var "String.get", [str; idx])
    | StringSet (str, idx, ast) ->
        aux env @@ AppCls (Var "String.set", [str; idx; ast])
    | ArrayGet (ary, idx) ->
        (* a.(b) returns b-th item of array a.
         * Therefore, convert it to Array.get call *)
        aux env @@ AppCls (Var "Array.get", [ary; idx])
    | TryWith (cond, cases) ->
        TryWith (aux env cond, analyze_pattern_match_cases env cases)
    | MatchWith (cond, cases) ->
        MatchWith (aux env cond, analyze_pattern_match_cases env cases)
    | Var name -> (
      match find_symbol env name with
      | 0, (Var _ as sym) -> sym
      | _, FuncVar (gen_funcname, 0) -> AppDir (gen_funcname, [])
      | 0, FuncVar (funcname, nargs) ->
          (* When FuncVar is processed here, AppDir will not be applied to this FuncVar.
           * Therefore the returned value should be closured in case
           * AppCls is applied to this value. *)
          MakeCls (funcname, nargs, [])
      | _, (Var id as sym) ->
          env.freevars := (name, id) :: !(env.freevars) ;
          sym
      | _ -> failwith @@ sprintf "not found variable in analysis: %s" name )
    | CtorApp (None, ctorname, None) ->
        let name_prefix, typename =
          try hashtbl_find_with_modulename toplevel.ctors_type ctorname
          with Not_found -> hashtbl_find_with_modulename toplevel.exps ctorname
        in
        CtorApp (Some typename, name_prefix ^ ctorname, None)
    | TypeAnd entries ->
        toplevel.typedefs
        <- List.rev_append toplevel.typedefs
           @@ List.map
                (function
                  | DefTypeAlias (type_param, typename, typ) ->
                      let typename = with_modulename typename in
                      DefTypeAlias (type_param, typename, typ)
                  | DefVariant (type_param, typename, ctornames) ->
                      let typename = with_modulename typename in
                      let ctornames =
                        List.map
                          (fun (ctorname, typexpr) ->
                            (with_modulename ctorname, typexpr) )
                          ctornames
                      in
                      List.iter
                        (fun (ctorname, _) ->
                          Hashtbl.add toplevel.ctors_type ctorname typename )
                        ctornames ;
                      DefVariant (type_param, typename, ctornames)
                  | DefRecord (typename, fields) ->
                      let typename = with_modulename typename in
                      let fields =
                        List.map
                          (fun (fieldname, typexpr) ->
                            (with_modulename fieldname, typexpr) )
                          fields
                      in
                      List.iter
                        (fun (fieldname, _) ->
                          Hashtbl.add toplevel.records fieldname typename )
                        fields ;
                      Hashtbl.add toplevel.records_fields typename
                      @@ List.map (fun (fieldname, _) -> fieldname) fields ;
                      DefRecord (typename, fields))
                entries ;
        Nope
    | ExpDef (expname, components) ->
        Hashtbl.add toplevel.exps expname expname ;
        toplevel.exps_list <- expname :: toplevel.exps_list ;
        Nope
    | OpenModuleDef modname ->
        let _, modname =
          hashtbl_find_with_modulename toplevel.modules (modname ^ ".")
        in
        toplevel.opened_modulename
        <- with_modulename modname :: modname :: toplevel.opened_modulename ;
        Nope
    | AppCls ((CtorApp (None, ctorname, None) as ctor), args) -> (
      match aux env ctor with
      | CtorApp (typename, ctorname, None) when List.length args = 1 ->
          CtorApp (typename, ctorname, Some (aux env @@ List.hd args))
      | _ -> failwith "invalid CtorApp" )
    | AppCls ((Var funcname as var), args) -> (
      try
        match
          match find_symbol env funcname with
          (* the symbol is 'safe' when it's in the same env
           * or it can be called by its name *)
          | 0, sym | _, (FuncVar _ as sym) -> sym
          | _, (Var id as sym) ->
              env.freevars := (funcname, id) :: !(env.freevars) ;
              sym
          | _ ->
              failwith @@ sprintf "not found variable in analysis: %s" funcname
        with
        | FuncVar (gen_funcname, nargs) ->
            let args = List.map (fun x -> aux env x) args in
            if List.length args = nargs then AppDir (gen_funcname, args)
            else
              let rec split n lst =
                if n = 0 then ([], lst)
                else
                  match lst with
                  | x :: xs ->
                      let lhs, rhs = split (n - 1) xs in
                      (x :: lhs, rhs)
                  | [] ->
                      failwith
                      @@ sprintf
                           "arguments to %s are too little: maybe curring?"
                           gen_funcname
              in
              let head, tail = split nargs args in
              AppCls (AppDir (gen_funcname, head), tail)
        | Var varname ->
            AppCls (aux env var, List.map (fun x -> aux env x) args)
        | _ -> raise Not_found
      with Not_found ->
        failwith (sprintf "not found in analysis (AppCls): %s" funcname) )
    | AppCls (func, args) ->
        AppCls (aux env func, List.map (fun x -> aux env x) args)
    | ForLoop (dir, indexname, expr1, expr2, expr3) ->
        let gen_indexname = make_id indexname in
        let env' =
          { env with
            symbols= Hashmap.add indexname (Var gen_indexname) env.symbols }
        in
        let expr1 = aux env expr1 in
        let expr2 = aux env expr2 in
        let expr3 = aux env' expr3 in
        ForLoop (dir, gen_indexname, expr1, expr2, expr3)
    | LetAnd (recursive, lhs_of_in, rhs_of_in) ->
        (* Split rhs_of_eq into LetVar and LetFunc. At the same time,
         * make a conversion table for function names *)
        let rec bind_with_modulename = function
          | ( IntValue _ | CharValue _ | UnitValue | EmptyList | PtnRange _
            | StringValue _ ) as ptn ->
              ptn
          | TupleValue values ->
              TupleValue (List.map (fun x -> bind_with_modulename x) values)
          | Cons (car, cdr) ->
              Cons (bind_with_modulename car, bind_with_modulename cdr)
          | PtnAlias (ptn, (Var _ as var)) ->
              PtnAlias (bind_with_modulename ptn, bind_with_modulename var)
          | PtnOr (lhs, rhs) ->
              PtnOr (bind_with_modulename lhs, bind_with_modulename rhs)
          | CtorApp (None, ctorname, arg) ->
              let arg =
                match arg with
                | Some arg -> Some (bind_with_modulename arg)
                | _ -> None
              in
              CtorApp (None, ctorname, arg)
          | Var name ->
              (* This process is the key. In this function,
               * we put the current module name to the defined variables *)
              Var (with_modulename name)
          | _ -> failwith "unexpected pattern"
        in
        let funcnames2gen = Hashtbl.create 2 in
        let src =
          List.map
            (function
              | [Var funcname], rhs_of_eq when recursive ->
                  (* When recursive, LetVar should be LetFunc with no arguments. *)
                  (* TODO:
                    If the lhs doesn't have any freevars, then there is no need to convert it.
                    Also, we should check whether the lhs uses itself in a correct way e.g.
                        let rec length = function x :: xs -> 1 + length xs | [] -> 0;;
                    is okay, but
                        let rec id x = id;;
                    is ng. For now, we assume that 'let rec ...' expression is written properly.
                  *)
                  let funcname =
                    match rhs_of_in with
                    | Some _ -> funcname
                    | None -> with_modulename funcname
                  in
                  Hashtbl.add funcnames2gen funcname (make_id funcname) ;
                  LetFunc (true, funcname, [], rhs_of_eq, [])
              | [bind], rhs_of_eq ->
                  let bind =
                    match rhs_of_in with
                    | None -> bind_with_modulename bind
                    | Some _ -> bind
                  in
                  LetVar (recursive, bind, rhs_of_eq)
              | Var funcname :: args, rhs_of_eq ->
                  let funcname =
                    match rhs_of_in with
                    | Some _ -> funcname
                    | None -> with_modulename funcname
                  in
                  Hashtbl.add funcnames2gen funcname (make_id funcname) ;
                  LetFunc (recursive, funcname, args, rhs_of_eq, [])
              | _ -> failwith "unexpected ast")
            lhs_of_in
        in
        (* Now, analyze all LetVar/LetFunc.
         * When we analyze *recursive* LetFunc, we must decide whether
         * we should call this function by name or as closure in itself.
         * Therefore, first, we assume that we can call them by name i.e. we use FuncVar.
         * Next, if we find we can't do so (i.e. there are any freevars), we decide to call them as closure,
         * that is, use Var, and analyze it again.
         * I (ushitora-anqou) 'pakutta' or borrowed this idea from MinCaml.
         * TODO: is there better way?*)
        let let_closures_freevars = ref [] in
        let should_be_closure = ref false in
        let rec analyze_lets first =
          let toplevel_letfuncs_backup = toplevel.letfuncs in
          let toplevel_strings_backup = toplevel.strings in
          let funcvars =
            hashmap_of_list
            @@ filter_after_map
                 (function
                   | LetFunc (_, funcname, args, _, _) ->
                       let gen_funcname =
                         Hashtbl.find funcnames2gen funcname
                       in
                       Some
                         ( if first then
                           (funcname, FuncVar (gen_funcname, List.length args))
                         else (funcname, Var gen_funcname) )
                   | _ -> None)
                 src
          in
          let rec aux' env' = function
            | LetVar (false, bind, lhs) ->
                let env' =
                  {env' with symbols= add_symbols_in_pattern env'.symbols bind}
                in
                (env', LetVar (false, aux_ptn env' bind, aux env lhs))
            | LetFunc (recursive, funcname, args, func, _) ->
                let gen_funcname = Hashtbl.find funcnames2gen funcname in
                let env_in =
                  { symbols= add_symbols_in_patterns Hashmap.empty args
                  ; parent= Some env
                  ; freevars= ref [] }
                in
                (* if recursive then funcname(s) should be in env *)
                let env_in =
                  if not recursive then env_in
                  else {env_in with symbols= integrate env_in.symbols funcvars}
                in
                let func = aux env_in func in
                (* Delete duplicate freevars *)
                env_in.freevars := list_unique !(env_in.freevars) ;
                let freevars =
                  ref (List.map (fun (_, a) -> a) !(env_in.freevars))
                in
                if first then (
                  (* Save data for the possible second loop *)
                  let_closures_freevars := !freevars @ !let_closures_freevars ;
                  (* If the function is recursive and should call itself as a closure,
                   * then Var should be used rather than FuncVar *)
                  if recursive && List.length !freevars <> 0 then
                    should_be_closure := true ;
                  if !should_be_closure then raise Should_be_closure ) ;
                let func =
                  if first then func
                  else (
                    (* In the target function, all functions chained with keyword 'and' should be available.
                     * This means that they should be defined as closures at the head of the target function.
                     * Note that these closures should have *all* freevars in chained functions. *)
                    (* TODO: only functions appeared in freevars need to be available. *)
                    freevars := !let_closures_freevars ;
                    LetAndAnalyzed
                      ( filter_after_map
                          (function
                            | LetFunc (_, funcname, args, _, _) ->
                                let gen_funcname =
                                  Hashtbl.find funcnames2gen funcname
                                in
                                Some
                                  (LetVar
                                     ( false
                                     , Var gen_funcname
                                     , MakeCls
                                         ( gen_funcname
                                         , List.length args
                                         , !let_closures_freevars ) ))
                            | _ -> None)
                          src
                      , func ) )
                in
                (* freevars are passed to env if they are not defined in env *)
                List.iter
                  (fun ((name, _) as var) ->
                    let d, _ = find_symbol env name in
                    if d <> 0 then env.freevars := var :: !(env.freevars) )
                  !(env_in.freevars) ;
                if List.length !freevars = 0 then (
                  (* no freevars; no need for closure *)
                  let env_out =
                    { env' with
                      symbols=
                        Hashmap.add funcname
                          (FuncVar (gen_funcname, List.length args))
                          env'.symbols }
                  in
                  let ast =
                    LetFunc
                      ( recursive
                      , gen_funcname
                      , List.map (fun x -> aux_ptn env_in x) args
                      , func
                      , [] )
                  in
                  toplevel.letfuncs <- ast :: toplevel.letfuncs ;
                  (env_out, ast) )
                else
                  (* closure *)
                  let funcvar = Var gen_funcname in
                  let env_out =
                    { env' with
                      symbols= Hashmap.add funcname funcvar env'.symbols }
                  in
                  let ast =
                    LetFunc
                      ( recursive
                      , gen_funcname
                      , List.map (fun x -> aux_ptn env_in x) args
                      , func
                      , !freevars )
                  in
                  toplevel.letfuncs <- ast :: toplevel.letfuncs ;
                  ( env_out
                  , LetVar
                      ( false
                      , funcvar
                      , MakeCls (gen_funcname, List.length args, !freevars) )
                  )
            | _ -> raise Unexpected_ast
          in
          let env', lets =
            List.fold_left
              (fun (env', lets) le ->
                try
                  match le with
                  | LetVar _ ->
                      let env', le_analyzed = aux' env' le in
                      (env', le_analyzed :: lets)
                  | LetFunc _ ->
                      let env', le_analyzed = aux' env' le in
                      (env', le_analyzed :: lets)
                  | _ -> failwith "unexpected ast"
                with Should_be_closure when first -> (env', lets) )
              (env, []) src
          in
          if first && !should_be_closure then (
            toplevel.letfuncs <- toplevel_letfuncs_backup ;
            toplevel.strings <- toplevel_strings_backup ;
            let_closures_freevars := list_unique !let_closures_freevars ;
            analyze_lets false )
          else
            match rhs_of_in with
            | None -> raise (LetDef (lets, env'))
            | Some rhs -> LetAndAnalyzed (lets, aux env' rhs)
        in
        analyze_lets true
    | _ -> raise Unexpected_ast
  and analyze_module env exprs =
    let toplevel_env = ref env in
    let rec aux' exprs = function
      | ModuleDef (this_modulename, body) :: asts ->
          toplevel.modulename <- this_modulename :: toplevel.modulename ;
          (* TODO: is there any better way? *)
          aux' exprs @@ body @ (ModuleDefEnd :: asts)
      | ModuleDefEnd :: asts ->
          let full_modname = get_modulename_prefix toplevel.modulename in
          Hashtbl.add toplevel.modules full_modname full_modname ;
          toplevel.modulename <- List.tl toplevel.modulename ;
          aux' exprs asts
      | ExternalDecl (id, typexpr, decl) :: asts ->
          let id = with_modulename id in
          let nargs =
            let rec aux cnt = function
              | TyFunc (lhs, rhs) -> aux (cnt + 1) rhs
              | _ -> cnt
            in
            aux 0 typexpr
          in
          toplevel_env :=
            { !toplevel_env with
              symbols=
                Hashmap.add id (FuncVar (decl, nargs)) !toplevel_env.symbols } ;
          aux' exprs asts
      | ast :: asts -> (
        try aux' (aux !toplevel_env ast :: exprs) asts
        with LetDef (lets, env) ->
          toplevel_env := env ;
          exprs2expr @@ List.rev
          @@ (LetAndAnalyzed (lets, aux' [] asts) :: exprs) )
      | [] -> exprs2expr @@ List.rev exprs
    in
    let ast = aux' [] exprs in
    (!toplevel_env, ast)
  in
  let env = {symbols= Hashmap.empty; parent= None; freevars= ref []} in
  let _, ast = analyze_module env asts in
  let ast = LetFunc (false, "aqaml_main", [UnitValue], ast, []) in
  toplevel.letfuncs <- ast :: toplevel.letfuncs ;
  (toplevel.letfuncs, toplevel.strings, toplevel.typedefs, toplevel.exps_list)

type gen_environment = {offset: int; varoffset: (string, int) Hashmap.t}

type ctype = CTyInt | CTyUnit | CTyPtr

type tail_recursive = Tail | NonTail

let rec generate (letfuncs, strings, typedefs, exps) =
  let stack_size = ref 0 in
  let new_offset env size =
    let offset = env.offset - (8 * size) in
    stack_size := max !stack_size (-offset) ;
    offset
  in
  let records_idx = Hashtbl.create 16 in
  let ctors_id = Hashtbl.create 16 in
  List.iter
    (function
      | DefTypeAlias _ -> ()
      | DefVariant (_, typename, ctornames) ->
          List.iteri
            (fun i (ctorname, _) -> Hashtbl.add ctors_id (typename, ctorname) i)
            ctornames
      | DefRecord (typename, fields) ->
          List.iteri
            (fun i (fieldname, _) ->
              Hashtbl.add records_idx (typename, fieldname) i )
            fields)
    typedefs ;
  let exps_id = Hashtbl.create 16 in
  List.iter
    (fun expname -> Hashtbl.add exps_id expname @@ Hashtbl.length exps_id)
    exps ;
  let reg_of_index idx =
    [|"rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"; "r12"; "r13"|].(idx)
  in
  let tag_int reg = sprintf "lea %s, [%s + %s + 1]" reg reg reg in
  let untag_int reg = sprintf "sar %s, 1" reg in
  let tagged_int num = (num lsl 1) lor 1 in
  let rec gen_alloc_block size color tag =
    (* allocated block address is in rax *)
    let buf = Buffer.create 128 in
    appfmt buf "mov rdi, %d" size ;
    appfmt buf "mov rsi, %d" color ;
    appfmt buf "mov rdx, %d" tag ;
    appstr buf "call aqaml_alloc_block@PLT" ;
    Buffer.contents buf
  in
  let rec gen_assign_pattern env exp_label = function
    | UnitValue | EmptyList -> gen_assign_pattern env exp_label @@ IntValue 0
    | IntValue num ->
        let buf = Buffer.create 128 in
        let exit_label = make_label () in
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int num ;
        appfmt buf "je %s" exit_label ;
        appfmt buf "jmp %s" exp_label ;
        appfmt buf "%s:" exit_label ;
        Buffer.contents buf
    | CharValue ch ->
        gen_assign_pattern env exp_label @@ IntValue (Char.code ch)
    | StringValue (id, str) ->
        let buf = Buffer.create 128 in
        appstr buf "pop rax" ;
        appfmt buf "lea rbx, [rip + %s]" id ;
        appstr buf "call aqaml_structural_equal" ;
        appstr buf "cmp rax, 1" ;
        appfmt buf "je %s" exp_label ;
        Buffer.contents buf
    | Var varname ->
        let buf = Buffer.create 128 in
        let offset = Hashmap.find varname env.varoffset in
        appstr buf "pop rax" ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        Buffer.contents buf
    | Cons (car, cdr) ->
        let buf = Buffer.create 128 in
        appstr buf "pop rax" ;
        appstr buf "cmp rax, 1" ;
        appfmt buf "je %s" exp_label ;
        appstr buf "push QWORD PTR [rax]" ;
        appstr buf "push QWORD PTR [rax + 8]" ;
        appstr buf @@ gen_assign_pattern env exp_label cdr ;
        appstr buf @@ gen_assign_pattern env exp_label car ;
        Buffer.contents buf
    | TupleValue values ->
        let buf = Buffer.create 128 in
        appstr buf "pop rax" ;
        List.iteri
          (fun i _ -> appfmt buf "push QWORD PTR [rax + %d]" (i * 8))
          values ;
        List.iter
          (fun x -> appstr buf @@ gen_assign_pattern env exp_label x)
          (List.rev values) ;
        Buffer.contents buf
    | CtorApp (Some typename, ctorname, None) ->
        gen_assign_pattern env exp_label
        @@ IntValue
             ( try Hashtbl.find ctors_id (typename, ctorname)
               with Not_found -> Hashtbl.find exps_id typename )
    | CtorApp (Some typename, ctorname, Some arg) ->
        let buf = Buffer.create 128 in
        let id =
          try Hashtbl.find ctors_id (typename, ctorname) with Not_found ->
            Hashtbl.find exps_id typename
        in
        appfmt buf "pop rax" ;
        appstr buf "mov rdi, rax" ;
        appstr buf "and rax, 1" ;
        appstr buf "cmp rax, 0" ;
        appfmt buf "jne %s" exp_label ;
        appstr buf "mov rax, [rdi - 8]" ;
        appstr buf "and rax, 0xff" ;
        appfmt buf "cmp rax, %d" id ;
        appfmt buf "jne %s" exp_label ;
        appstr buf "push [rdi]" ;
        appstr buf @@ gen_assign_pattern env exp_label arg ;
        Buffer.contents buf
    | PtnAlias (ptn, Var varname) ->
        let buf = Buffer.create 128 in
        let offset = Hashmap.find varname env.varoffset in
        appstr buf "pop rax" ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        appstr buf "push rax" ;
        appstr buf @@ gen_assign_pattern env exp_label ptn ;
        Buffer.contents buf
    | PtnOr (lhs, rhs) ->
        let next_label = make_label () in
        let exit_label = make_label () in
        let saved_rsp_offset = new_offset env 1 in
        let env = {env with offset= saved_rsp_offset} in
        let buf = Buffer.create 128 in
        appfmt buf "mov [rbp + %d], rsp" saved_rsp_offset ;
        appstr buf "pop rax" ;
        appstr buf "push rax" ;
        appstr buf "push rax" ;
        appstr buf @@ gen_assign_pattern env next_label lhs ;
        appstr buf "pop rax" ;
        appfmt buf "jmp %s" exit_label ;
        appfmt buf "%s:" next_label ;
        appfmt buf "mov rsp, [rbp + %d]" saved_rsp_offset ;
        appstr buf @@ gen_assign_pattern env exp_label rhs ;
        appfmt buf "%s:" exit_label ;
        Buffer.contents buf
    | PtnRange (bg, ed) ->
        let buf = Buffer.create 128 in
        let exit0_label = make_label () in
        let exit1_label = make_label () in
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int @@ Char.code bg ;
        appfmt buf "jge %s" exit0_label ;
        appfmt buf "jmp %s" exp_label ;
        appfmt buf "%s:" exit0_label ;
        appfmt buf "cmp rax, %d" @@ tagged_int @@ Char.code ed ;
        appfmt buf "jle %s" exit1_label ;
        appfmt buf "jmp %s" exp_label ;
        appfmt buf "%s:" exit1_label ;
        Buffer.contents buf
    | _ -> raise Unexpected_ast
  in
  let rec gen_assign_pattern_or_raise env ptn =
    let exp_label = make_label () in
    let exit_label = make_label () in
    let assign_code = gen_assign_pattern env exp_label ptn in
    let buf = Buffer.create 256 in
    appstr buf assign_code ;
    appfmt buf "jmp %s" exit_label ;
    appfmt buf "%s:" exp_label ;
    (* TODO: arguments of Match_failure *)
    appstr buf "mov rax, 1" ;
    appstr buf @@ gen_raise_exp_of "Match_failure" true ;
    appfmt buf "%s:" exit_label ;
    Buffer.contents buf
  and gen_raise () =
    let buf = Buffer.create 128 in
    (* Raise. Thanks to:
     * https://github.com/ocamllabs/ocaml-multicore/wiki/Native-code-notes *)
    appstr buf "mov rsp, r14" ;
    appstr buf "pop r14" ;
    appstr buf "ret" ;
    Buffer.contents buf
  and gen_raise_exp_of expname has_arguments =
    let buf = Buffer.create 128 in
    appstr buf "/* raise */" ;
    if not has_arguments then
      appfmt buf "mov rax, %d" @@ tagged_int @@ Hashtbl.find exps_id expname
    else (
      (* Assume that the argument is stored in rax *)
      appstr buf "mov rbx, rax" ;
      appstr buf @@ gen_alloc_block 1 0 @@ Hashtbl.find exps_id expname ;
      appstr buf "mov [rax], rbx" ) ;
    appstr buf @@ gen_raise () ;
    Buffer.contents buf
  and gen_pattern_match_cases env cases istail exp_body =
    (* Assume that the target value is in stack top *)
    let buf = Buffer.create 128 in
    let saved_rsp_offset = new_offset env 1 in
    let env = {env with offset= saved_rsp_offset} in
    appfmt buf "mov [rbp + %d], rsp" saved_rsp_offset ;
    let exit_label = make_label () in
    let exp_label =
      List.fold_left
        (fun this_label (ptn, whn, case) ->
          let varnames = varnames_in_pattern ptn in
          let offset = new_offset env @@ List.length varnames in
          let env =
            { offset
            ; varoffset=
                (let rec aux i varoffset = function
                   | varname :: varnames ->
                       aux (i + 1)
                         (Hashmap.add varname (env.offset - (i * 8)) varoffset)
                         varnames
                   | [] -> varoffset
                 in
                 aux 1 env.varoffset varnames) }
          in
          let next_label = make_label () in
          appfmt buf "%s:" this_label ;
          appfmt buf "mov rsp, [rbp + %d]" saved_rsp_offset ;
          appstr buf "pop rax" ;
          appstr buf "push rax" ;
          appstr buf "push rax" ;
          appstr buf @@ gen_assign_pattern env next_label ptn ;
          ( match whn with
          | None -> ()
          | Some expr ->
              appstr buf @@ aux env (NonTail, expr) ;
              appstr buf "pop rax" ;
              appfmt buf "cmp rax, %d" @@ tagged_int 0 ;
              appfmt buf "je %s" next_label ) ;
          appstr buf "pop rax" ;
          appstr buf @@ aux env (istail, case) ;
          appfmt buf "jmp %s /* exit label */" exit_label ;
          next_label )
        (make_label ()) cases
    in
    appfmt buf "%s:" exp_label ;
    appstr buf "/* match failed */" ;
    appstr buf exp_body ;
    appfmt buf "%s:" exit_label ;
    Buffer.contents buf
  and aux env = function
    | _, Nope -> "push 0 /* dummy */"
    | _, IntValue num -> sprintf "push %d" (tagged_int num)
    | istail, CharValue ch -> aux env (istail, IntValue (Char.code ch))
    | istail, (UnitValue | EmptyList) -> aux env (istail, IntValue 0)
    | _, StringValue (id, _) -> sprintf "lea rax, [rip + %s]\npush rax" id
    | _, Cons (car, cdr) ->
        let buf = Buffer.create 128 in
        appstr buf "/* Cons BEGIN */" ;
        appstr buf @@ aux env (NonTail, cdr) ;
        appstr buf @@ aux env (NonTail, car) ;
        appstr buf @@ gen_alloc_block 2 0 0 ;
        appstr buf "pop rdi /* car */" ;
        appstr buf "mov [rax], rdi" ;
        appstr buf "pop rdi /* cdr */" ;
        appstr buf "mov [rax + 8], rdi" ;
        appstr buf "push rax" ;
        appstr buf "/* Cons END */" ;
        Buffer.contents buf
    | _, TupleValue values ->
        let size = List.length values in
        let buf = Buffer.create 128 in
        appstr buf @@ "/* TupleValue BEGIN */" ;
        List.iter
          (fun x -> appstr buf @@ aux env (NonTail, x))
          (List.rev values) ;
        appstr buf @@ gen_alloc_block size 0 1 ;
        List.iteri
          (fun i _ -> appfmt buf "pop rdi\nmov [rax + %d], rdi" (i * 8))
          values ;
        appstr buf @@ "push rax" ;
        appstr buf @@ "/* TupleValue END */" ;
        Buffer.contents buf
    | _, ArrayValue values ->
        let size = List.length values in
        let buf = Buffer.create 128 in
        appstr buf @@ "/* ArrayValue BEGIN */" ;
        List.iter
          (fun x -> appstr buf @@ aux env (NonTail, x))
          (List.rev values) ;
        appstr buf @@ gen_alloc_block size 0 1 ;
        List.iteri
          (fun i _ -> appfmt buf "pop rdi\nmov [rax + %d], rdi" (i * 8))
          values ;
        appstr buf @@ "push rax" ;
        appstr buf @@ "/* ArrayValue END */" ;
        Buffer.contents buf
    | _, RecordValue (Some typename, fields) ->
        let offset = new_offset env 1 in
        let buf = Buffer.create 128 in
        appfmt buf "/* RecordValue %s BEGIN */" typename ;
        appstr buf @@ gen_alloc_block (List.length fields) 0 1 ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        List.iter
          (fun (fieldname, ast) ->
            appstr buf @@ aux env (NonTail, ast) ;
            appstr buf "pop rax" ;
            appfmt buf "mov rdi, [rbp + %d]" offset ;
            let idx = Hashtbl.find records_idx (typename, fieldname) in
            appfmt buf "mov [rdi + %d], rax" (idx * 8) )
          fields ;
        appfmt buf "push [rbp + %d]" offset ;
        appfmt buf "/* RecordValue %s END */" typename ;
        Buffer.contents buf
    | _, RecordDotAccess (Some typename, ast, fieldname) ->
        let idx = Hashtbl.find records_idx (typename, fieldname) in
        let buf = Buffer.create 128 in
        appfmt buf "/* RecordDotAccess %s %s */" typename fieldname ;
        appstr buf @@ aux env (NonTail, ast) ;
        appstr buf "pop rax" ;
        appfmt buf "push [rax + %d]" (idx * 8) ;
        Buffer.contents buf
    | _, Add (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf @@ "pop rdi" ;
        appstr buf @@ untag_int "rdi" ;
        appstr buf @@ "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf @@ "add rax, rdi" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf @@ "push rax" ;
        Buffer.contents buf
    | _, Sub (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf @@ "pop rdi" ;
        appstr buf @@ untag_int "rdi" ;
        appstr buf @@ "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf @@ "sub rax, rdi" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf @@ "push rax" ;
        Buffer.contents buf
    | _, Mul (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf @@ "pop rdi" ;
        appstr buf @@ untag_int "rdi" ;
        appstr buf @@ "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf @@ "imul rax, rdi" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf @@ "push rax" ;
        Buffer.contents buf
    | _, Div (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rdi" ;
        appstr buf @@ untag_int "rdi" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "cqo" ;
        appstr buf "idiv rdi" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, Rem (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rdi" ;
        appstr buf @@ untag_int "rdi" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "cqo" ;
        appstr buf "idiv rdi" ;
        appstr buf @@ tag_int "rdx" ;
        appstr buf "push rdx" ;
        Buffer.contents buf
    | _, LogicalLeftShift (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rcx" ;
        appstr buf @@ untag_int "rcx" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "shl rax, cl" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, LogicalRightShift (lhs, rhs) ->
        (* Note that the size of int is 63bit, not 64bit. *)
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rcx" ;
        appstr buf @@ untag_int "rcx" ;
        appstr buf "pop rax" ;
        appstr buf "shr rax, cl" ;
        appstr buf "or rax, 1" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, ArithmeticRightShift (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rcx" ;
        appstr buf @@ untag_int "rcx" ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "sar rax, cl" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, BitwiseAnd (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rdi" ;
        appstr buf "pop rax" ;
        appstr buf "and rax, rdi" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, BitwiseOr (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rdi" ;
        appstr buf "pop rax" ;
        appstr buf "or rax, rdi" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, StringConcat (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appstr buf "call aqaml_concat_string" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, ListConcat (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appstr buf "call aqaml_concat_list" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, RefAssign (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appstr buf "mov [rax], rbx" ;
        (* push unit value *)
        appstr buf "push 1" ;
        Buffer.contents buf
    | _, RecordAssign (Some typename, lhs, fieldname, rhs) ->
        let idx = Hashtbl.find records_idx (typename, fieldname) in
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appfmt buf "mov [rax + %d], rbx" (idx * 8) ;
        (* push unit value *)
        appstr buf "push 1" ;
        Buffer.contents buf
    | _, Deref ast ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, ast) ;
        appstr buf "pop rax" ;
        appstr buf "push [rax]" ;
        Buffer.contents buf
    | _, Positate ast -> ""
    | _, Negate ast ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, ast) ;
        appstr buf "pop rax" ;
        appstr buf @@ untag_int "rax" ;
        appstr buf "neg rax" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, StructEqual (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf @@ "pop rbx" ;
        appstr buf @@ "pop rax" ;
        appstr buf @@ "call aqaml_structural_equal" ;
        appstr buf @@ "push rax" ;
        Buffer.contents buf
    | _, StructInequal (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rbx" ;
        appstr buf "pop rax" ;
        appstr buf "call aqaml_structural_inequal" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, LessThan (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rdi" ;
        appstr buf "pop rax" ;
        appstr buf "cmp rax, rdi" ;
        appstr buf "setl al" ;
        appstr buf "movzx rax, al" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, LessThanEqual (lhs, rhs) ->
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rdi" ;
        appstr buf "pop rax" ;
        appstr buf "cmp rax, rdi" ;
        appstr buf "setle al" ;
        appstr buf "movzx rax, al" ;
        appstr buf @@ tag_int "rax" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | _, LogicalAnd (lhs, rhs) ->
        let false_label = make_label () in
        let exit_label = make_label () in
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int 0 ;
        appfmt buf "je %s" false_label ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int 0 ;
        appfmt buf "je %s" false_label ;
        appfmt buf "push %d" @@ tagged_int 1 ;
        appfmt buf "jmp %s" exit_label ;
        appfmt buf "%s:" false_label ;
        appfmt buf "push %d" @@ tagged_int 0 ;
        appfmt buf "%s:" exit_label ;
        Buffer.contents buf
    | _, LogicalOr (lhs, rhs) ->
        let true_label = make_label () in
        let exit_label = make_label () in
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, lhs) ;
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int 1 ;
        appfmt buf "je %s" true_label ;
        appstr buf @@ aux env (NonTail, rhs) ;
        appstr buf "pop rax" ;
        appfmt buf "cmp rax, %d" @@ tagged_int 1 ;
        appfmt buf "je %s" true_label ;
        appfmt buf "push %d" @@ tagged_int 0 ;
        appfmt buf "jmp %s" exit_label ;
        appfmt buf "%s:" true_label ;
        appfmt buf "push %d" @@ tagged_int 1 ;
        appfmt buf "%s:" exit_label ;
        Buffer.contents buf
    | istail, IfThenElse (cond, then_body, else_body) ->
        let false_label = make_label () in
        let exit_label = make_label () in
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, cond) ;
        appstr buf "pop rax" ;
        appstr buf "cmp rax, 1" (* if rax = 0 then then_body else else_body *) ;
        appfmt buf "je %s" false_label ;
        appstr buf @@ aux env (istail, then_body) ;
        appfmt buf "jmp %s" exit_label ;
        appfmt buf "%s:" false_label ;
        ( match else_body with
        | None ->
            appstr buf @@ aux env (istail, IntValue 0)
            (* unit value is IntValue 0 *)
        | Some else_body -> appstr buf @@ aux env (istail, else_body) ) ;
        appfmt buf "%s:" exit_label ;
        Buffer.contents buf
    | istail, ExprSeq exprs ->
        String.concat "\npop rax\n"
          (List.mapi
             (fun i x ->
               aux env
                 ((if i = List.length exprs - 1 then istail else NonTail), x)
               )
             exprs)
    | _, Var varname -> (
      try
        let offset = Hashmap.find varname env.varoffset in
        let buf = Buffer.create 128 in
        appfmt buf "mov rax, [rbp + %d]" offset ;
        appstr buf "push rax" ;
        Buffer.contents buf
      with Not_found ->
        failwith (sprintf "not found in code generation: %s" varname) )
    | istail, CtorApp (Some typename, ctorname, None) ->
        aux env
        @@ ( istail
           , IntValue
               ( try Hashtbl.find ctors_id (typename, ctorname)
                 with Not_found -> Hashtbl.find exps_id typename ) )
    | _, CtorApp (Some typename, ctorname, Some arg) ->
        let buf = Buffer.create 128 in
        appstr buf
        @@ gen_alloc_block 1 0
             ( try Hashtbl.find ctors_id (typename, ctorname)
               with Not_found -> Hashtbl.find exps_id typename ) ;
        appstr buf "push rax" ;
        appstr buf @@ aux env (NonTail, arg) ;
        appstr buf "pop rdi" ;
        appstr buf "pop rax" ;
        appfmt buf "mov [rax], rdi" ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | istail, AppDir (funcname, args) ->
        let buf = Buffer.create 128 in
        List.iter
          (fun arg -> appstr buf @@ aux env (NonTail, arg))
          (List.rev args) ;
        List.iteri
          (fun index reg ->
            if index < List.length args then appfmt buf "pop %s" reg )
          ["rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"; "r12"; "r13"] ;
        ( match istail with
        | NonTail ->
            appfmt buf "call %s" funcname ;
            appstr buf "push rax"
        | Tail ->
            (* TODO: arguments passed via stack *)
            appstr buf "mov rsp, rbp" ;
            appstr buf "pop rbp" ;
            appfmt buf "jmp %s" funcname ) ;
        Buffer.contents buf
    | istail, AppCls (func, args) ->
        (* call aqaml_appcls *)
        (* TODO: Any better way exists? *)
        (* TODO: only 9 or less arguments are allowed *)
        if List.length args > 9 then
          failwith "only 9 or less arguments are allowed (not implemented)" ;
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, func) ;
        List.iter
          (fun arg -> appstr buf @@ aux env (NonTail, arg))
          (List.rev args) ;
        List.iteri
          (fun index reg ->
            if index < List.length args then appfmt buf "pop %s" reg )
          ["rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"; "r12"; "r13"] ;
        appstr buf "pop rax" ;
        ( match istail with
        | NonTail ->
            appfmt buf "call aqaml_appcls%d" @@ List.length args ;
            appstr buf "push rax"
        | Tail ->
            (* TODO: arguments passed via stack *)
            appstr buf "mov rsp, rbp" ;
            appstr buf "pop rbp" ;
            appfmt buf "jmp aqaml_appcls%d" @@ List.length args ) ;
        Buffer.contents buf
    | _, ForLoop (dir, indexname, expr1, expr2, expr3) ->
        let loop_label = make_label () in
        let exit_label = make_label () in
        let offset = new_offset env 1 in
        let env' =
          {offset; varoffset= Hashmap.add indexname offset env.varoffset}
        in
        let buf = Buffer.create 128 in
        appstr buf @@ aux env (NonTail, expr2) ;
        appstr buf @@ aux env (NonTail, expr1) ;
        appstr buf "pop rax" ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        appstr buf "pop rax" ;
        appfmt buf "cmp [rbp + %d], rax" offset ;
        appfmt buf "%s %s"
          (match dir with ForTo -> "jg" | ForDownto -> "jl")
          exit_label ;
        appstr buf "push rax" ;
        appfmt buf "%s:" loop_label ;
        appfmt buf "pop rax" ;
        appfmt buf "cmp [rbp + %d], rax" offset ;
        appfmt buf "%s %s"
          (match dir with ForTo -> "jg" | ForDownto -> "jl")
          exit_label ;
        appstr buf "push rax" ;
        appstr buf @@ aux env' (NonTail, expr3) ;
        appstr buf "pop rax /* pop unit value */" ;
        ( match dir with
        | ForTo -> appfmt buf "add QWORD PTR [rbp + %d], 2" offset
        | ForDownto -> appfmt buf "sub QWORD PTR [rbp + %d], 2" offset ) ;
        appfmt buf "jmp %s" loop_label ;
        appfmt buf "%s:" exit_label ;
        appfmt buf "push %d /* push unit value */" @@ tagged_int 0 ;
        Buffer.contents buf
    | istail, LetAndAnalyzed (lets, rhs_of_in) ->
        let buf = Buffer.create 256 in
        let aux' env' = function
          | LetVar (false, bind, lhs) ->
              let varnames = varnames_in_pattern bind in
              let offset = new_offset env' @@ List.length varnames in
              let env' =
                { offset
                ; varoffset=
                    integrate env'.varoffset @@ hashmap_of_list
                    @@ List.mapi
                         (fun i n -> (n, env'.offset - ((i + 1) * 8)))
                         varnames }
              in
              appstr buf @@ aux env (NonTail, lhs) ;
              appstr buf @@ gen_assign_pattern_or_raise env' bind ;
              env'
          | LetFunc (_, funcname, _, _, _) ->
              let offset = new_offset env' 1 in
              let env' =
                {offset; varoffset= Hashmap.add funcname offset env'.varoffset}
              in
              env'
          | _ -> raise Unexpected_ast
        in
        let env' = List.fold_left (fun env le -> aux' env le) env lets in
        appstr buf @@ aux env' (istail, rhs_of_in) ;
        Buffer.contents buf
    | _, MakeCls (funcname, nargs, freevars) ->
        let buf = Buffer.create 128 in
        appstr buf @@ gen_alloc_block (List.length freevars + 2) 0 247 ;
        appfmt buf "lea rdi, [rip + %s]" funcname ;
        appstr buf "mov [rax], rdi" ;
        appfmt buf "mov QWORD PTR [rax + 8], %d" @@ nargs ;
        List.iteri
          (fun i var ->
            let offset = Hashmap.find var env.varoffset in
            appfmt buf "mov rdi, [rbp + %d]" offset ;
            appfmt buf "mov [rax + %d], rdi" ((i + 2) * 8) )
          freevars ;
        appstr buf "push rax" ;
        Buffer.contents buf
    | istail, MatchWith (cond, cases) ->
        let buf = Buffer.create 256 in
        appstr buf "/* MatchWith BEGIN */" ;
        appstr buf @@ aux env (NonTail, cond) ;
        appstr buf
        @@ gen_pattern_match_cases env cases istail
             (let buf = Buffer.create 128 in
              appstr buf "mov rax, 1" ;
              (* TODO: arguments for Match_failure *)
              appstr buf @@ gen_raise_exp_of "Match_failure" true ;
              Buffer.contents buf) ;
        appstr buf "/* MatchWith END */" ;
        Buffer.contents buf
    | istail, TryWith (cond, cases) ->
        let offset = new_offset env 1 in
        let env = {env with offset} in
        let exp_label = make_label () in
        let exit_label = make_label () in
        let buf = Buffer.create 256 in
        appstr buf "/* TryWith BEGIN */" ;
        (* set an exception handler *)
        appfmt buf "lea r13, [rip + %s]" exp_label ;
        appstr buf "push rbp" ;
        appstr buf "push r13" ;
        appstr buf "push r14" ;
        appstr buf "mov r14, rsp" ;
        appstr buf @@ aux env (NonTail, cond) ;
        appstr buf "pop rax" ;
        appstr buf "pop r14 /* pop for r14 */" ;
        appstr buf "pop rbx /* pop for r13 */" ;
        appstr buf "pop rbx /* pop for rbp */" ;
        appstr buf "push rax" ;
        appfmt buf "jmp %s" exit_label ;
        appfmt buf "%s:" exp_label ;
        appstr buf "pop rbp" ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        appstr buf "push rax" ;
        appstr buf
        @@ gen_pattern_match_cases env cases istail
             (let buf = Buffer.create 128 in
              appfmt buf "mov rax, [rbp + %d]" offset ;
              appstr buf @@ gen_raise () ;
              Buffer.contents buf) ;
        appfmt buf "%s:" exit_label ;
        appstr buf "/* TryWith END */" ;
        Buffer.contents buf
    | _ -> raise Unexpected_ast
  in
  let strings_code =
    let buf = Buffer.create 80 in
    appfmt buf ".data" ;
    List.iter
      (function
        | StringValue (id, str) ->
            let size = (String.length str / 8) + 1 in
            let space = 7 - (String.length str mod 8) in
            appfmt buf ".quad %d" ((size lsl 10) lor (0 lsl 8) lor 252) ;
            appfmt buf "%s:" id ;
            appfmt buf ".ascii \"%s\"" (escape_string str) ;
            if space <> 0 then appfmt buf ".space %d" space ;
            appfmt buf ".byte %d\n" space
        | _ -> raise Unexpected_ast)
      strings ;
    appfmt buf ".text\n" ;
    Buffer.contents buf
  in
  let letfuncs_code =
    String.concat "\n"
      (List.map
         (function
           | LetFunc (recursive, funcname, args, func, freevars) ->
               let varnames =
                 List.flatten @@ List.map varnames_in_pattern args
               in
               let env =
                 { offset= List.length varnames * -8
                 ; varoffset=
                     integrate Hashmap.empty @@ hashmap_of_list
                     @@ List.mapi (fun i arg -> (arg, -8 * (i + 1))) varnames
                 }
               in
               (* if closured then freevars are stored on the stack *)
               let env =
                 { offset= env.offset - (8 * List.length freevars)
                 ; varoffset=
                     integrate env.varoffset @@ hashmap_of_list
                     @@ List.mapi
                          (fun i var -> (var, env.offset - (8 * (i + 1))))
                          freevars }
               in
               stack_size := -env.offset ;
               let code = aux env (Tail, func) in
               let buf = Buffer.create 256 in
               appfmt buf "/* %s(%d) */"
                 (if recursive then "recursive" else "")
                 (List.length args) ;
               appstr buf @@ funcname ^ ":" ;
               appstr buf @@ "push rbp" ;
               appstr buf @@ "mov rbp, rsp" ;
               appfmt buf "sub rsp, %d" !stack_size ;
               (* push arguments in order *)
               (* first the value for closure *)
               if List.length freevars <> 0 then
                 appfmt buf "push %s" @@ reg_of_index @@ List.length args ;
               (* then real arguments *)
               appstr buf @@ String.concat "\n" @@ List.rev
               @@ List.mapi
                    (fun i _ -> sprintf "push %s" (reg_of_index i))
                    args ;
               (* process real arguments *)
               appstr buf @@ String.concat "\n"
               @@ List.map
                    (fun ptn -> gen_assign_pattern_or_raise env ptn)
                    args ;
               (* process for closure *)
               if List.length freevars > 0 then (
                 appstr buf "pop rax" ;
                 List.iteri
                   (fun i var ->
                     appfmt buf "mov rdi, [rax + %d]" (i * 8) ;
                     appfmt buf "mov [rbp + %d], rdi"
                     @@ Hashmap.find var env.varoffset )
                   freevars ) ;
               appstr buf code ;
               appstr buf "pop rax" ;
               appstr buf "mov rsp, rbp" ;
               appstr buf "pop rbp" ;
               appstr buf "ret\n\n" ;
               Buffer.contents buf
           | _ -> failwith "LetFunc should be here")
         letfuncs)
  in
  let main_code =
    let buf = Buffer.create 512 in
    let gen_c_func funcname argument_types ret_type =
      appfmt buf "%s:" funcname ;
      List.iteri
        (fun i -> function
          | CTyInt | CTyUnit -> appstr buf @@ untag_int @@ reg_of_index i
          | _ -> () )
        argument_types ;
      for i = List.length argument_types - 1 downto 0 do
        appfmt buf "mov %s"
        @@
        match i with
        | 0 -> "rdi, rax"
        | 1 -> "rsi, rbx"
        | 2 -> "rdx, rdi"
        | 3 -> "rcx, rsi"
        | 4 -> "r8, rdx"
        | 5 -> "r9, rcx"
        | _ ->
            failwith "C function with more than 6 arguments can't be handled."
      done ;
      appfmt buf "call %s_detail@PLT" funcname ;
      ( match ret_type with
      | CTyInt -> appstr buf @@ tag_int "rax"
      | CTyUnit -> appfmt buf "mov rax, %d" @@ tagged_int 0
      | CTyPtr -> () ) ;
      appstr buf "ret" ; appstr buf ""
    in
    gen_c_func "aqaml_malloc" [CTyInt] CTyPtr ;
    gen_c_func "aqaml_structural_equal" [CTyPtr; CTyPtr] CTyInt ;
    gen_c_func "aqaml_concat_string" [CTyPtr; CTyPtr] CTyPtr ;
    gen_c_func "aqaml_concat_list" [CTyPtr; CTyPtr] CTyPtr ;
    gen_c_func "aqaml_string_length" [CTyPtr] CTyInt ;
    gen_c_func "aqaml_string_get" [CTyPtr; CTyInt] CTyInt ;
    gen_c_func "aqaml_string_set" [CTyPtr; CTyInt; CTyInt] CTyInt ;
    gen_c_func "aqaml_array_get" [CTyPtr; CTyInt] CTyPtr ;
    gen_c_func "aqaml_array_length" [CTyPtr] CTyInt ;
    gen_c_func "aqaml_string_create" [CTyInt] CTyPtr ;
    gen_c_func "aqaml_string_blit"
      [CTyPtr; CTyInt; CTyPtr; CTyInt; CTyInt]
      CTyUnit ;
    gen_c_func "aqaml_string_sub" [CTyPtr; CTyInt; CTyInt] CTyPtr ;
    gen_c_func "aqaml_string_make" [CTyInt; CTyInt] CTyPtr ;
    gen_c_func "aqaml_string_of_int" [CTyInt] CTyPtr ;
    gen_c_func "aqaml_print_string" [CTyPtr] CTyUnit ;
    gen_c_func "aqaml_prerr_string" [CTyPtr] CTyUnit ;
    gen_c_func "aqaml_printf_ksprintf" [CTyPtr; CTyPtr] CTyPtr ;
    appstr buf
      ".global aqaml_printf_ksprintf1, aqaml_printf_ksprintf2, \
       aqaml_printf_ksprintf3, aqaml_printf_ksprintf4, aqaml_printf_ksprintf5" ;
    gen_c_func "aqaml_printf_ksprintf1" [CTyPtr; CTyPtr] CTyPtr ;
    gen_c_func "aqaml_printf_ksprintf2" [CTyPtr; CTyPtr; CTyPtr] CTyPtr ;
    gen_c_func "aqaml_printf_ksprintf3" [CTyPtr; CTyPtr; CTyPtr; CTyPtr] CTyPtr ;
    gen_c_func "aqaml_printf_ksprintf4"
      [CTyPtr; CTyPtr; CTyPtr; CTyPtr; CTyPtr]
      CTyPtr ;
    gen_c_func "aqaml_printf_ksprintf5"
      [CTyPtr; CTyPtr; CTyPtr; CTyPtr; CTyPtr; CTyPtr]
      CTyPtr ;
    gen_c_func "aqaml_get_stdin" [CTyUnit] CTyPtr ;
    gen_c_func "aqaml_close_in" [CTyPtr] CTyUnit ;
    appstr buf "aqaml_input_char:" ;
    let exit_label = make_label () in
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_input_char_detail@PLT" ;
    appstr buf "cmp rax, -1" ;
    appfmt buf "jne %s" exit_label ;
    appstr buf @@ gen_raise_exp_of "End_of_file" false ;
    appfmt buf "%s:" exit_label ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_open_in:" ;
    let exit_label = make_label () in
    appstr buf "mov rdi, rax" ;
    appstr buf "call aqaml_open_in_detail@PLT" ;
    appstr buf "cmp rax, 0" ;
    appfmt buf "jne %s" exit_label ;
    (* TODO: raise 'No such file or directory *)
    appstr buf "mov rax, 0" ;
    appstr buf @@ gen_raise_exp_of "Sys_error" true ;
    appfmt buf "%s:" exit_label ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_structural_inequal:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf "call aqaml_structural_equal_detail@PLT" ;
    appstr buf "test eax, eax" ;
    appstr buf "sete al" ;
    appstr buf @@ tag_int "rax" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_char_code:" ;
    appstr buf "aqaml_char_chr:" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_exit:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "shr rdi, 1" ;
    appstr buf "call exit@PLT" ;
    appstr buf "" ;
    appstr buf "aqaml_ref:" ;
    appstr buf "mov rbx, rax" ;
    appstr buf @@ gen_alloc_block 1 0 0 ;
    appstr buf "mov [rax], rbx" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_raise:" ;
    appstr buf @@ gen_raise () ;
    appstr buf "" ;
    for nargs = 1 to 10 do
      appfmt buf ".global aqaml_call_func%d" nargs ;
      appfmt buf "aqaml_call_func%d:" nargs ;
      appstr buf "mov r10, rdi" ;
      if nargs >= 1 then appstr buf "mov rax, rsi" ;
      if nargs >= 2 then appstr buf "mov rbx, rdx" ;
      if nargs >= 3 then appstr buf "mov rdi, rcx" ;
      if nargs >= 4 then appstr buf "mov rsi, r8" ;
      if nargs >= 5 then appstr buf "mov rdx, r9" ;
      if nargs >= 6 then appstr buf "mov rcx, [rsp + 8]" ;
      if nargs >= 7 then appstr buf "mov r8, [rsp + 16]" ;
      if nargs >= 8 then appstr buf "mov r9, [rsp + 24]" ;
      if nargs >= 9 then appstr buf "mov r12, [rsp + 32]" ;
      if nargs >= 10 then appstr buf "mov r13, [rsp + 40]" ;
      appstr buf "jmp r10" ;
      appstr buf ""
    done ;
    (* emit aqaml_appcls%d *)
    for nargs = 1 to 9 do
      let label_loop = make_label () in
      let label_exit = make_label () in
      let label_ret = make_label () in
      appfmt buf "aqaml_appcls%d:" nargs ;
      appstr buf "push rbp" ;
      appstr buf "mov rbp, rsp" ;
      appstr buf "sub rsp, 16" ;
      for i = nargs downto 1 do
        appfmt buf "push %s" @@ reg_of_index i
      done ;
      appfmt buf "mov QWORD PTR [rbp - 8], %d" nargs ;
      appfmt buf "%s:" label_loop ;
      appstr buf "mov r10, rax" ;
      appstr buf "mov r11, [rax + 8]" ;
      appstr buf "sub [rbp - 8], r11" ;
      appstr buf "lea rax, [r10 + 16]" ;
      for i = 0 to nargs - 1 do
        appstr buf "cmp r11, 0" ;
        appfmt buf "je %s" label_exit ;
        appstr buf "dec r11" ;
        appfmt buf "pop %s" @@ reg_of_index i ;
        appfmt buf "lea %s, [r10 + 16]" @@ reg_of_index (i + 1)
      done ;
      appfmt buf "%s:" label_exit ;
      appstr buf "cmp QWORD PTR [rbp - 8], 0" ;
      appfmt buf "je %s" label_ret ;
      appstr buf "call [r10]" ;
      appfmt buf "jmp %s" label_loop ;
      appfmt buf "%s:" label_ret ;
      appstr buf "mov rsp, rbp" ;
      appstr buf "pop rbp" ;
      appstr buf "jmp [r10]" ;
      appstr buf ""
    done ;
    appstr buf "aqaml_get_argv:" ;
    appstr buf "mov rax, [rip + aqaml_sys_argv]" ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf ".global main" ;
    appstr buf "main:" ;
    appstr buf "push rbp" ;
    appstr buf "mov rbp, rsp" ;
    (* handle command-line arguments *)
    appstr buf "push rsi" ;
    appstr buf "push rdi" ;
    appstr buf "mov rsi, 0" ;
    appstr buf "mov rdx, 1" ;
    appstr buf "call aqaml_alloc_block@PLT" ;
    appstr buf "pop rdi" ;
    appstr buf "pop rsi" ;
    let exit_label = make_label () in
    let loop_label = make_label () in
    appfmt buf "%s:" loop_label ;
    appstr buf "cmp rdi, 0" ;
    appfmt buf "je %s" exit_label ;
    appstr buf "dec rdi" ;
    appstr buf "mov rcx, [rsi + rdi * 8]" ;
    appstr buf "push rax" ;
    appstr buf "push rdi" ;
    appstr buf "push rsi" ;
    appstr buf "mov rdi, rcx" ;
    appstr buf "call aqaml_create_string_from_cstr@PLT" ;
    appstr buf "mov rcx, rax" ;
    appstr buf "pop rsi" ;
    appstr buf "pop rdi" ;
    appstr buf "pop rax" ;
    appstr buf "mov [rax + rdi * 8], rcx" ;
    appfmt buf "jmp %s" loop_label ;
    appfmt buf "%s:" exit_label ;
    appstr buf ".data" ;
    appstr buf "aqaml_sys_argv:" ;
    appstr buf ".space 8" ;
    appstr buf ".text" ;
    appstr buf "mov [rip + aqaml_sys_argv], rax" ;
    (* default exception handler *)
    appstr buf "lea r13, [rip + aqaml_default_exception_handler]" ;
    appstr buf "push r13" ;
    appstr buf "push r14" ;
    appstr buf "mov r14, rsp" ;
    (* give unit value as an argument *)
    appfmt buf "mov rax, %d" @@ tagged_int 0 ;
    appstr buf "call aqaml_main" ;
    appstr buf "pop rax" ;
    appstr buf "pop rax" ;
    appstr buf "mov rax, 0" ;
    appstr buf "pop rbp" ;
    appstr buf "ret" ;
    appstr buf "aqaml_default_exception_handler:" ;
    appfmt buf "mov rax, %d" @@ tagged_int 1 ;
    appstr buf "call aqaml_exit" ;
    appstr buf "" ;
    Buffer.contents buf
  in
  main_code ^ letfuncs_code ^ strings_code

let input_lines inch =
  let rec aux lines =
    try
      let line = input_line inch in
      aux (line :: lines)
    with End_of_file -> lines
  in
  String.concat "\n" (List.rev (aux []))

let read_file path =
  let inch = open_in path in
  let str = input_lines inch in
  close_in inch ; str

let get_modulename_from_path path =
  let rec aux i fi ti =
    if i < 0 then
      String.init
        (ti - fi + 1)
        (fun i ->
          let idx = i + fi in
          if i = 0 && path.[idx] >= 'a' then
            Char.chr @@ (Char.code path.[idx] - 32)
          else path.[idx] )
    else
      match path.[i] with
      | '.' -> aux (i - 1) (i - 1) (i - 1)
      | '/' -> aux (-1) fi ti
      | _ -> aux (i - 1) i ti
  in
  let i = String.length path - 1 in
  aux i i i

;;
try
  let buf = Buffer.create 128 in
  Array.iteri
    (fun i path ->
      if i >= 1 then
        appfmt buf ";;module %s = struct %s end;;"
          (get_modulename_from_path path)
          (read_file path) )
    Sys.argv ;
  Buffer.contents buf |> tokenize |> parse |> analyze |> generate
  |> printf ".intel_syntax noprefix\n%s"
with Failure str -> eprintf "[AQaml Error] %s\n" @@ str
