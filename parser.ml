open Lexer

type typ =
  | TyInt
  | TyChar
  | TyUnit
  | TyBool
  | TyString
  | TyTuple of typ list
  | TyCustom of string
  | TyVar of string
  | TyCtorApp of typ * string
  | TyArgs of typ list
  | TyFunc of typ * typ

type ast =
  | UnitValue
  | IntValue of int
  | CharValue of char
  | StringValue of string * string
  | TupleValue of ast list
  | ArrayValue of ast list
  | RecordValue of string option * (string * ast) list
  | RecordValueWith of
      string option * ast * (string * ast) list * string list option
  | RecordDotAccess of string option * ast * string
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Rem of ast * ast
  | LogicalLeftShift of ast * ast
  | LogicalRightShift of ast * ast
  | ArithmeticRightShift of ast * ast
  | LogicalAnd of ast * ast
  | LogicalOr of ast * ast
  | BitwiseAnd of ast * ast
  | BitwiseOr of ast * ast
  | StringConcat of ast * ast
  | ListConcat of ast * ast
  | Negate of ast
  | Positate of ast
  | StructEqual of (ast * ast)
  | StructInequal of (ast * ast)
  | LessThan of (ast * ast)
  | LessThanEqual of (ast * ast)
  | IfThenElse of (ast * ast * ast option)
  | Var of string
  | FuncVar of string * int
  | AppCls of (ast * ast list)
  | AppDir of (string * ast list)
  | LetAnd of (bool * (pattern list * ast) list * ast option)
  | LetVar of (bool * pattern * ast)
  (* recursive?, funcname, args, function body, free variables *)
  | LetFunc of (bool * string * pattern list * ast * string list)
  | LetAndAnalyzed of ast list * ast
  | Cons of (ast * ast)
  | EmptyList
  | ExprSeq of ast list
  | MatchWith of ast * (pattern * ast option * ast) list
  | MakeCls of string * int * string list
  | Lambda of pattern list * ast
  | TypeAnd of typedef list
  | CtorApp of string option * string * ast option
  | RefAssign of ast * ast
  | RecordAssign of string option * ast * string * ast
  | Deref of ast
  | ExpDef of string * typ option
  | TryWith of ast * (pattern * ast option * ast) list
  | StringGet of ast * ast
  | StringSet of ast * ast * ast
  | ArrayGet of ast * ast
  | ForLoop of for_loop_dir * string * ast * ast * ast
  | Nope
  | ModuleDef of string * ast list
  (* for analysis *)
  | ModuleDefEnd
  | ExternalDecl of string * typ * string
  | OpenModuleDef of string
  (* TODO: module Ptn *)
  | PtnOr of pattern * pattern
  | PtnAlias of pattern * ast
  | PtnRange of char * char

and pattern = ast

and typedef =
  | DefVariant of typ option * string * (string * typ option) list
  | DefTypeAlias of typ option * string * typ
  | DefRecord of string * (string * typ) list

and for_loop_dir = ForTo | ForDownto

exception Unexpected_ast

let rec varnames_in_pattern = function
  (* TODO: much faster algorithm? *)
  | UnitValue | IntValue _ | CharValue _ | StringValue _ | EmptyList
   |PtnRange _ ->
      []
  | Var varname -> [varname]
  | Cons (car, cdr) ->
      List.rev_append (varnames_in_pattern car) (varnames_in_pattern cdr)
  | TupleValue values ->
      List.fold_left
        (fun a b -> List.rev_append a (varnames_in_pattern b))
        [] values
  | CtorApp (_, _, None) -> []
  | CtorApp (_, _, Some arg) -> varnames_in_pattern arg
  | PtnOr (lhs, rhs) ->
      List.rev_append (varnames_in_pattern lhs) (varnames_in_pattern rhs)
  | PtnAlias (ptn, Var name) -> name :: varnames_in_pattern ptn
  | _ -> raise Unexpected_ast

let parse tokens =
  let is_primary = function
    | ( IntLiteral _ | CharLiteral _ | StringLiteral _ | LowerIdent _
      | LowerIdentWithModule _ | CapitalIdent _ | CapitalIdentWithModule _
      | LRBracket | NarutoNaruto | LParen | LBracket | LRParen | LBrace )
      :: _ ->
        true
    | _ -> false
  in
  let is_dot = function
    | (Dot | DotLBracket | DotLParen) :: _ -> true
    | _ -> false
  in
  let is_prefix = function Exclam :: _ -> true | _ -> false in
  let is_let = function
    | (Function | Fun | Match | Try | Let) :: _ -> true
    | _ -> false
  in
  let is_if = function If :: _ -> true | _ -> false in
  let rec parse_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | CharLiteral ch :: tokens -> (tokens, CharValue ch)
    | StringLiteral (id, str) :: tokens -> (tokens, StringValue (id, str))
    | LRParen :: tokens -> (tokens, UnitValue)
    | (LowerIdentWithModule varname | LowerIdent varname) :: tokens ->
        (tokens, Var varname)
    | (CapitalIdentWithModule ctorname | CapitalIdent ctorname) :: tokens ->
        (tokens, CtorApp (None, ctorname, None))
    | LRBracket :: tokens -> (tokens, EmptyList)
    | NarutoNaruto :: tokens -> parse_let tokens
    | LParen :: tokens -> (
        let tokens, ast = parse_expression tokens in
        match tokens with
        | RParen :: tokens -> (tokens, ast)
        | x -> raise_unexpected_token x )
    | LBracketBar :: tokens ->
        let rec aux lst = function
          | Semicolon :: tokens ->
              let tokens, item = parse_let tokens in
              aux (item :: lst) tokens
          | BarRBracket :: tokens -> (tokens, ArrayValue (List.rev lst))
          | x -> raise_unexpected_token x
        in
        let tokens, item = parse_let tokens in
        aux [item] tokens
    | LBracket :: tokens ->
        let rec aux = function
          | Semicolon :: tokens ->
              let tokens, car = parse_let tokens in
              let tokens, cdr = aux tokens in
              (tokens, Cons (car, cdr))
          | RBracket :: tokens -> (tokens, EmptyList)
          | x -> raise_unexpected_token x
        in
        let tokens, car = parse_let tokens in
        let tokens, cdr = aux tokens in
        (tokens, Cons (car, cdr))
    | LBrace :: tokens -> (
        let rec parse_record_fields first fields tokens =
          let aux fieldname = function
            | Equal :: tokens -> parse_let tokens
            | (Semicolon | RBrace) :: _ as tokens -> (tokens, Var fieldname)
            | x -> raise_unexpected_token x
          in
          match tokens with
          | (LowerIdent fieldname | LowerIdentWithModule fieldname) :: tokens
            when first ->
              let tokens, ast = aux fieldname tokens in
              parse_record_fields false ((fieldname, ast) :: fields) tokens
          | Semicolon
            :: (LowerIdent fieldname | LowerIdentWithModule fieldname)
               :: tokens
            when not first ->
              let tokens, ast = aux fieldname tokens in
              parse_record_fields false ((fieldname, ast) :: fields) tokens
          | RBrace :: tokens -> (tokens, fields)
          | x -> raise_unexpected_token x
        in
        match tokens with
        | (LowerIdent _ | LowerIdentWithModule _) :: (Equal | Semicolon) :: _
          ->
            let tokens, fields = parse_record_fields true [] tokens in
            (tokens, RecordValue (None, fields))
        | _ -> (
            let tokens, base = parse_prefix tokens in
            match tokens with
            | With :: tokens ->
                let tokens, fields = parse_record_fields true [] tokens in
                (tokens, RecordValueWith (None, base, fields, None))
            | x -> raise_unexpected_token x ) )
    | x -> raise_unexpected_token x
  and parse_prefix = function
    | Exclam :: tokens ->
        let tokens, ast = parse_primary tokens in
        (tokens, Deref ast)
    | tokens -> parse_primary tokens
  and parse_dot tokens =
    let tokens, lhs = parse_prefix tokens in
    match tokens with
    | Dot :: LowerIdent fieldname :: tokens ->
        (tokens, RecordDotAccess (None, lhs, fieldname))
    | DotLBracket :: tokens -> (
        let tokens, rhs = parse_expression tokens in
        match tokens with
        | RBracket :: tokens -> (tokens, StringGet (lhs, rhs))
        | x -> raise_unexpected_token x )
    | DotLParen :: tokens -> (
        let tokens, rhs = parse_expression tokens in
        match tokens with
        | RParen :: tokens -> (tokens, ArrayGet (lhs, rhs))
        | x -> raise_unexpected_token x )
    | _ -> (tokens, lhs)
  and parse_funccall tokens =
    let rec aux tokens =
      if is_primary tokens || is_dot tokens || is_prefix tokens then
        let tokens, arg = parse_dot tokens in
        let tokens, args = aux tokens in
        (tokens, arg :: args)
      else (tokens, [])
    in
    let tokens, func = parse_dot tokens in
    let tokens, args = aux tokens in
    if args = [] then (tokens, func) (* not function call *)
    else (tokens, AppCls (func, args))
  and parse_unary = function
    | Minus :: tokens ->
        let tokens, ast = parse_unary tokens in
        (tokens, Negate ast)
    | Plus :: tokens ->
        let tokens, ast = parse_unary tokens in
        (tokens, Positate ast)
    | tokens -> parse_funccall tokens
  and parse_shift tokens =
    let rec aux lhs = function
      | Lsl :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (LogicalLeftShift (lhs, rhs)) tokens
      | Lsr :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (LogicalRightShift (lhs, rhs)) tokens
      | Asr :: tokens ->
          let tokens, rhs = parse_unary tokens in
          aux (ArithmeticRightShift (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, lhs = parse_unary tokens in
    aux lhs tokens
  and parse_multiplicative tokens =
    let rec aux lhs = function
      | Star :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Mul (lhs, rhs)) tokens
      | Slash :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Div (lhs, rhs)) tokens
      | Mod :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (Rem (lhs, rhs)) tokens
      | Land :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (BitwiseAnd (lhs, rhs)) tokens
      | Lor :: tokens ->
          let tokens, rhs = parse_shift tokens in
          aux (BitwiseOr (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, ast = parse_shift tokens in
    aux ast tokens
  and parse_additive tokens =
    let rec aux lhs tokens =
      match tokens with
      | Plus :: tokens ->
          let tokens, rhs = parse_multiplicative tokens in
          aux (Add (lhs, rhs)) tokens
      | Minus :: tokens ->
          let tokens, rhs = parse_multiplicative tokens in
          aux (Sub (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_multiplicative tokens in
    aux ast tokens
  and parse_cons tokens =
    let tokens, car = parse_additive tokens in
    match tokens with
    | ColonColon :: tokens ->
        let tokens, cdr = parse_cons tokens in
        (tokens, Cons (car, cdr))
    | _ -> (tokens, car)
  and parse_string_concat tokens =
    let rec aux lhs tokens =
      match tokens with
      | Hat :: tokens ->
          let tokens, rhs = parse_cons tokens in
          aux (StringConcat (lhs, rhs)) tokens
      | Naruto :: tokens ->
          let tokens, rhs = parse_cons tokens in
          aux (ListConcat (lhs, rhs)) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_cons tokens in
    aux ast tokens
  and parse_structural_equal tokens =
    let rec aux lhs tokens =
      match tokens with
      | Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (StructEqual (lhs, rhs)) tokens
      | LTGT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (StructInequal (lhs, rhs)) tokens
      | LT :: Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThanEqual (lhs, rhs)) tokens
      | GT :: Equal :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThanEqual (rhs, lhs)) tokens
      | LT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThan (lhs, rhs)) tokens
      | GT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux (LessThan (rhs, lhs)) tokens
      | PipeGT :: tokens ->
          let tokens, rhs = parse_string_concat tokens in
          aux
            ( match rhs with
            | AppCls (func, args) ->
                AppCls (func, List.rev (lhs :: List.rev args))
            | _ -> AppCls (rhs, [lhs]) )
            tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_string_concat tokens in
    aux ast tokens
  and parse_logical_and tokens =
    let rec aux lhs = function
      | AndAnd :: tokens ->
          let tokens, rhs = parse_structural_equal tokens in
          aux (LogicalAnd (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, ast = parse_structural_equal tokens in
    aux ast tokens
  and parse_logical_or tokens =
    let rec aux lhs = function
      | PipePipe :: tokens ->
          let tokens, rhs = parse_logical_and tokens in
          aux (LogicalOr (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, ast = parse_logical_and tokens in
    aux ast tokens
  and parse_tuple tokens =
    let rec aux lhs tokens =
      match tokens with
      | Comma :: tokens ->
          let tokens, rhs =
            if is_let tokens || is_if tokens then parse_let tokens
            else parse_logical_or tokens
          in
          aux (rhs :: lhs) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_logical_or tokens in
    let tokens, ast_list = aux [ast] tokens in
    match ast_list with
    | [] -> raise_unexpected_token []
    | [ast] -> (tokens, ast)
    | asts -> (tokens, TupleValue (List.rev asts))
  and parse_assignment tokens =
    let tokens, lhs = parse_tuple tokens in
    match tokens with
    | ColonEqual :: tokens ->
        let tokens, rhs = parse_let tokens in
        (tokens, RefAssign (lhs, rhs))
    | LArrow :: tokens -> (
        let tokens, rhs = parse_let tokens in
        match lhs with
        | StringGet (str, idx) -> (tokens, StringSet (str, idx, rhs))
        | RecordDotAccess (None, lhs, fieldname) ->
            (tokens, RecordAssign (None, lhs, fieldname, rhs))
        | _ -> raise_unexpected_token tokens )
    | _ -> (tokens, lhs)
  and parse_if = function
    | If :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | Then :: tokens -> (
            let tokens, then_body = parse_let tokens in
            match tokens with
            | Else :: tokens ->
                let tokens, else_body = parse_let tokens in
                (tokens, IfThenElse (cond, then_body, Some else_body))
            | _ -> (tokens, IfThenElse (cond, then_body, None)) )
        | x -> raise_unexpected_token x )
    | tokens -> parse_assignment tokens
  and parse_pattern_match tokens =
    let rec aux first cases tokens =
      let aux' tokens =
        let tokens, ptn = parse_pattern tokens in
        let tokens, whn =
          match tokens with
          | When :: tokens ->
              let tokens, expr = parse_expression tokens in
              (tokens, Some expr)
          | _ -> (tokens, None)
        in
        match tokens with
        | Arrow :: tokens ->
            let tokens, case = parse_expression tokens in
            let tokens, cases = aux false ((ptn, whn, case) :: cases) tokens in
            (tokens, cases)
        | x -> raise_unexpected_token x
      in
      match tokens with
      | Pipe :: tokens -> aux' tokens
      | _ -> if first then aux' tokens else (tokens, List.rev cases)
    in
    aux true [] tokens
  and parse_let = function
    | Function :: tokens ->
        let argname = ".arg" in
        let tokens, cases = parse_pattern_match tokens in
        (tokens, Lambda ([Var argname], MatchWith (Var argname, cases)))
    | Fun :: tokens ->
        let rec aux = function
          | Arrow :: tokens -> (tokens, [])
          | tokens ->
              let tokens, arg = parse_pattern tokens in
              let tokens, args = aux tokens in
              (tokens, arg :: args)
        in
        let tokens, args = aux tokens in
        let tokens, func = parse_expression tokens in
        (tokens, Lambda (args, func))
    | Match :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | With :: tokens ->
            let tokens, cases = parse_pattern_match tokens in
            (tokens, MatchWith (cond, cases))
        | x -> raise_unexpected_token x )
    | Try :: tokens -> (
        let tokens, cond = parse_expression tokens in
        match tokens with
        | With :: tokens ->
            let tokens, cases = parse_pattern_match tokens in
            (tokens, TryWith (cond, cases))
        | x -> raise_unexpected_token x )
    | For :: LowerIdent indexname :: Equal :: tokens -> (
        let tokens, expr1 = parse_expression tokens in
        match tokens with
        | ((To | Downto) as dir) :: tokens -> (
            let tokens, expr2 = parse_expression tokens in
            match tokens with
            | Do :: tokens -> (
                let tokens, expr3 = parse_expression tokens in
                match tokens with
                | Done :: tokens ->
                    ( tokens
                    , ForLoop
                        ( (if dir = To then ForTo else ForDownto)
                        , indexname
                        , expr1
                        , expr2
                        , expr3 ) )
                | x -> raise_unexpected_token x )
            | x -> raise_unexpected_token x )
        | x -> raise_unexpected_token x )
    | Let :: tokens -> (
        let parse_let_binding tokens =
          let tokens, bind = parse_pattern tokens in
          match tokens with
          | Equal :: tokens ->
              (* define constants *)
              let tokens, lhs = parse_expression tokens in
              (tokens, ([bind], lhs))
          | _ ->
              (* define function *)
              let rec aux = function
                | Equal :: tokens -> (tokens, [])
                | tokens ->
                    let tokens, arg = parse_pattern tokens in
                    let tokens, args = aux tokens in
                    (tokens, arg :: args)
              in
              let tokens, args = aux tokens in
              let tokens, func = parse_expression tokens in
              (tokens, (bind :: args, func))
        in
        let tokens, recursive =
          match tokens with
          | Rec :: tokens -> (tokens, true)
          | _ -> (tokens, false)
        in
        let rec aux' lets = function
          | And :: tokens ->
              let tokens, le = parse_let_binding tokens in
              aux' (le :: lets) tokens
          | tokens -> (tokens, lets)
        in
        let tokens, le = parse_let_binding tokens in
        let tokens, lets = aux' [le] tokens in
        match tokens with
        | In :: tokens ->
            let tokens, rhs_of_in = parse_expression tokens in
            (tokens, LetAnd (recursive, lets, Some rhs_of_in))
        | _ -> (tokens, LetAnd (recursive, lets, None)) )
    | tokens -> parse_if tokens
  and parse_expr_sequence tokens =
    let rec aux = function
      | Semicolon :: tokens ->
          let tokens, expr = parse_let tokens in
          let tokens, exprs = aux tokens in
          (tokens, expr :: exprs)
      | tokens -> (tokens, [])
    in
    let tokens, expr = parse_let tokens in
    let tokens, exprs = aux tokens in
    if List.length exprs = 0 then (tokens, expr)
    else (tokens, ExprSeq (expr :: exprs))
  and parse_expression tokens = parse_expr_sequence tokens
  and parse_pattern_primary = function
    | IntLiteral num :: tokens -> (tokens, IntValue num)
    | CharLiteral ch :: tokens -> (tokens, CharValue ch)
    | StringLiteral (id, str) :: tokens -> (tokens, StringValue (id, str))
    | LRParen :: tokens -> (tokens, UnitValue)
    | (LowerIdent id | LowerIdentWithModule id) :: tokens -> (tokens, Var id)
    | (CapitalIdent id | CapitalIdentWithModule id) :: tokens ->
        (tokens, CtorApp (None, id, None))
    | LRBracket :: tokens -> (tokens, EmptyList)
    | LParen :: tokens -> (
        let tokens, ast = parse_pattern tokens in
        match tokens with
        | RParen :: tokens -> (tokens, ast)
        | x -> raise_unexpected_token x )
    | LBracket :: tokens ->
        let rec aux = function
          | Semicolon :: tokens ->
              let tokens, car = parse_pattern tokens in
              let tokens, cdr = aux tokens in
              (tokens, Cons (car, cdr))
          | RBracket :: tokens -> (tokens, EmptyList)
          | x -> raise_unexpected_token x
        in
        let tokens, car = parse_pattern tokens in
        let tokens, cdr = aux tokens in
        (tokens, Cons (car, cdr))
    | x -> raise_unexpected_token x
  and parse_pattern_range = function
    | CharLiteral st :: DotDot :: CharLiteral ed :: tokens ->
        (tokens, PtnRange (st, ed))
    | tokens -> parse_pattern_primary tokens
  and parse_pattern_ctor_app tokens =
    let tokens, ctorapp = parse_pattern_range tokens in
    match ctorapp with
    | CtorApp (None, ctorname, None) when is_primary tokens ->
        let tokens, arg = parse_pattern_range tokens in
        (tokens, CtorApp (None, ctorname, Some arg))
    | _ -> (tokens, ctorapp)
  and parse_pattern_cons tokens =
    let tokens, car = parse_pattern_ctor_app tokens in
    match tokens with
    | ColonColon :: tokens ->
        let tokens, cdr = parse_pattern_cons tokens in
        (tokens, Cons (car, cdr))
    | _ -> (tokens, car)
  and parse_pattern_tuple tokens =
    let rec aux lhs tokens =
      match tokens with
      | Comma :: tokens ->
          let tokens, rhs = parse_pattern_cons tokens in
          aux (rhs :: lhs) tokens
      | _ -> (tokens, lhs)
    in
    let tokens, ast = parse_pattern_cons tokens in
    let tokens, ast_list = aux [ast] tokens in
    match ast_list with
    | [] -> raise_unexpected_token []
    | [ast] -> (tokens, ast)
    | asts -> (tokens, TupleValue (List.rev asts))
  and parse_pattern_or tokens =
    let rec aux lhs = function
      | Pipe :: tokens ->
          let tokens, rhs = parse_pattern_tuple tokens in
          aux (PtnOr (lhs, rhs)) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, lhs = parse_pattern_tuple tokens in
    aux lhs tokens
  and parse_pattern_as tokens =
    let tokens, ptn = parse_pattern_or tokens in
    match tokens with
    | As :: LowerIdent name :: tokens -> (tokens, PtnAlias (ptn, Var name))
    | _ -> (tokens, ptn)
  and parse_pattern tokens = parse_pattern_as tokens
  and parse_typexpr_primary = function
    | KwInt :: tokens -> (tokens, TyInt)
    | KwChar :: tokens -> (tokens, TyChar)
    | KwUnit :: tokens -> (tokens, TyUnit)
    | KwBool :: tokens -> (tokens, TyBool)
    | KwString :: tokens -> (tokens, TyString)
    | Apostrophe :: LowerIdent id :: tokens -> (tokens, TyVar id)
    | (LowerIdent typename | LowerIdentWithModule typename) :: tokens ->
        (tokens, TyCustom typename)
    | LParen :: _ ->
        failwith "Any token LParen should be handled in parse_typexpr_ctor_app"
    | x -> raise_unexpected_token x
  and parse_typexpr_ctor_app tokens =
    let tokens, lhs =
      match tokens with
      | LParen :: tokens ->
          let tokens, typexpr = parse_typexpr tokens in
          let rec aux types = function
            | Comma :: tokens ->
                let tokens, typexpr = parse_typexpr tokens in
                aux (typexpr :: types) tokens
            | RParen :: tokens -> (tokens, types)
            | x -> raise_unexpected_token x
          in
          let tokens, types = aux [typexpr] tokens in
          let types = List.rev types in
          ( tokens
          , if List.length types = 1 then List.hd types else TyArgs types )
      | _ -> parse_typexpr_primary tokens
    in
    let rec aux lhs = function
      | (LowerIdent typectorname | LowerIdentWithModule typectorname) :: tokens
        ->
          aux (TyCtorApp (lhs, typectorname)) tokens
      | tokens -> (tokens, lhs)
    in
    aux lhs tokens
  and parse_typexpr_tuple tokens =
    let rec aux lhs = function
      | Star :: tokens ->
          let tokens, rhs = parse_typexpr_ctor_app tokens in
          aux (rhs :: lhs) tokens
      | tokens -> (tokens, lhs)
    in
    let tokens, typexpr = parse_typexpr_ctor_app tokens in
    let tokens, typexprs = aux [typexpr] tokens in
    match typexprs with
    | [] -> raise_unexpected_token []
    | [typexpr] -> (tokens, typexpr)
    | typexprs -> (tokens, TyTuple typexprs)
  and parse_typexpr_func tokens =
    let tokens, lhs = parse_typexpr_tuple tokens in
    match tokens with
    | Arrow :: tokens ->
        let tokens, rhs = parse_typexpr_func tokens in
        (tokens, TyFunc (lhs, rhs))
    | _ -> (tokens, lhs)
  and parse_typexpr tokens = parse_typexpr_func tokens
  and parse_type_def tokens =
    let parse_type_param = function
      | Apostrophe :: LowerIdent id :: tokens -> (tokens, TyVar id)
      | x -> raise_unexpected_token x
    in
    let parse_type_params = function
      | LParen :: tokens ->
          let rec aux type_params = function
            | Comma :: tokens ->
                let tokens, type_param = parse_type_param tokens in
                aux (type_param :: type_params) tokens
            | RParen :: tokens -> (tokens, TyTuple type_params)
            | x -> raise_unexpected_token x
          in
          let tokens, type_param = parse_type_param tokens in
          Some (aux [type_param] tokens)
      | Apostrophe :: _ as tokens -> Some (parse_type_param tokens)
      | _ -> None
    in
    let parse_type_def_entry tokens =
      let tokens, type_param =
        match parse_type_params tokens with
        | None -> (tokens, None)
        | Some (tokens, type_params) -> (tokens, Some type_params)
      in
      match tokens with
      | LowerIdent typename :: Equal :: tokens -> (
          let parse_variant tokens =
            let rec aux first ctors = function
              | Pipe :: CapitalIdent ctorname :: Of :: tokens ->
                  let tokens, typ = parse_typexpr tokens in
                  aux false ((ctorname, Some typ) :: ctors) tokens
              | CapitalIdent ctorname :: Of :: tokens when first ->
                  let tokens, typ = parse_typexpr tokens in
                  aux false ((ctorname, Some typ) :: ctors) tokens
              | Pipe :: CapitalIdent ctorname :: tokens ->
                  aux false ((ctorname, None) :: ctors) tokens
              | CapitalIdent ctorname :: tokens when first ->
                  aux false ((ctorname, None) :: ctors) tokens
              | tokens -> (tokens, ctors)
            in
            let tokens, ctors = aux true [] tokens in
            (tokens, DefVariant (type_param, typename, ctors))
          in
          match tokens with
          | CapitalIdent str :: _ -> parse_variant tokens
          | Pipe :: _ -> parse_variant tokens
          (* TODO: skip mutable *)
          | LBrace :: LowerIdent fieldname :: Colon :: tokens
           |LBrace :: Mutable :: LowerIdent fieldname :: Colon :: tokens ->
              let rec aux fields = function
                | Semicolon :: LowerIdent fieldname :: Colon :: tokens
                 |Semicolon
                  :: Mutable :: LowerIdent fieldname :: Colon :: tokens ->
                    let tokens, typexpr = parse_typexpr tokens in
                    aux ((fieldname, typexpr) :: fields) tokens
                | RBrace :: tokens -> (tokens, fields)
                | x -> raise_unexpected_token x
              in
              let tokens, typexpr = parse_typexpr tokens in
              let tokens, fields = aux [(fieldname, typexpr)] tokens in
              (tokens, DefRecord (typename, fields))
          | tokens ->
              let tokens, typ = parse_typexpr tokens in
              (tokens, DefTypeAlias (type_param, typename, typ)) )
      | x -> raise_unexpected_token x
    in
    let rec aux entries = function
      | And :: tokens ->
          let tokens, entry = parse_type_def_entry tokens in
          aux (entry :: entries) tokens
      | tokens -> (tokens, TypeAnd entries)
    in
    (* token Type is already fetched *)
    let tokens, entry = parse_type_def_entry tokens in
    aux [entry] tokens
  and parse_exp_def = function
    (* token Exception is already fetched *)
    | CapitalIdent expname :: Of :: tokens ->
        let tokens, typ = parse_typexpr tokens in
        (tokens, ExpDef (expname, Some typ))
    | CapitalIdent expname :: tokens -> (tokens, ExpDef (expname, None))
    | x -> raise_unexpected_token x
  in
  let parse_expressions_and_definitions tokens =
    (* Here are some tricks. All expressions split by double semicolons (;;)
     * are converted to (maybe large) one ExprSeq, and all 'let' without 'in'
     * come to have their following expressions as their 'in' part.
     * This change makes later processes such as semantic analysis easier. *)
    (* TODO: correct? *)
    (* TODO: not correct. definitions and expressions should be completely separated. *)
    let rec aux exprs = function
      | SemicolonSemicolon :: tokens -> aux exprs tokens
      | [] -> ([], List.rev exprs)
      | Type :: tokens ->
          let tokens, expr = parse_type_def tokens in
          aux (expr :: exprs) tokens
      | Exception :: tokens ->
          let tokens, expr = parse_exp_def tokens in
          aux (expr :: exprs) tokens
      | External :: LowerIdent id :: Colon :: tokens -> (
          let tokens, typexpr = parse_typexpr tokens in
          match tokens with
          | Equal :: StringLiteral (_, str) :: tokens ->
              let ast = ExternalDecl (id, typexpr, str) in
              aux (ast :: exprs) tokens
          | x -> raise_unexpected_token x )
      | Open :: CapitalIdent modname :: tokens
       |Open :: CapitalIdentWithModule modname :: tokens ->
          aux (OpenModuleDef modname :: exprs) tokens
      | Module :: CapitalIdent modulename :: Equal :: Struct :: tokens ->
          let tokens, asts = aux [] tokens in
          let ast = ModuleDef (modulename, asts) in
          aux (ast :: exprs) tokens
      | End :: tokens -> (* module end *)
                         (tokens, List.rev exprs)
      | tokens ->
          let tokens, expr = parse_expression tokens in
          aux (expr :: exprs) tokens
    in
    let _, exprs = aux [] tokens in
    exprs
  in
  parse_expressions_and_definitions tokens
