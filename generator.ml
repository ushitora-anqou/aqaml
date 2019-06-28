open Printf
open Helper
open Parser
open Analyzer

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
  let gen_call_with_aligned_rsp callee =
    let buf = Buffer.create 128 in
    let just_call_label = make_label () in
    let exit_label = make_label () in
    appstr buf "mov rax, rsp" ;
    appstr buf "and rax, 0x0f" ;
    appstr buf "cmp rax, 0" ;
    appfmt buf "je %s" just_call_label ;
    (* rsp is not 16-byte aligned BUT 8-byte aligned *)
    appstr buf "sub rsp, 8" ;
    appfmt buf "call %s" callee ;
    appstr buf "add rsp, 8" ;
    appfmt buf "jmp %s" exit_label ;
    appfmt buf "%s:" just_call_label ;
    appfmt buf "call %s" callee ;
    appfmt buf "%s:" exit_label ;
    Buffer.contents buf
  in
  let rec gen_alloc_block size color tag =
    (* allocated block address is in rax *)
    let buf = Buffer.create 128 in
    appfmt buf "mov rdi, %d" size ;
    appfmt buf "mov rsi, %d" color ;
    appfmt buf "mov rdx, %d" tag ;
    appstr buf @@ gen_call_with_aligned_rsp "aqaml_alloc_block@PLT" ;
    (* appstr buf "call aqaml_alloc_block@PLT" ; *)
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
    appstr buf @@ gen_raise_exp_of "Stdlib.Match_failure" true ;
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
        let env = {env with offset} in
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
    | _, RecordValueWith (Some typename, base, fields, Some comp_fieldnames) ->
        let offset = new_offset env 1 in
        let env = {env with offset} in
        let buf = Buffer.create 128 in
        appfmt buf "/* RecordValueWith %s BEGIN */" typename ;
        appstr buf
        @@ gen_alloc_block
             (List.length fields + List.length comp_fieldnames)
             0 1 ;
        appfmt buf "mov [rbp + %d], rax" offset ;
        List.iter
          (fun (fieldname, ast) ->
            appstr buf @@ aux env (NonTail, ast) ;
            appstr buf "pop rax" ;
            appfmt buf "mov rdi, [rbp + %d]" offset ;
            let idx = Hashtbl.find records_idx (typename, fieldname) in
            appfmt buf "mov [rdi + %d], rax" (idx * 8) )
          fields ;
        appstr buf @@ aux env (NonTail, base) ;
        appstr buf "pop rax" ;
        appfmt buf "mov rdi, [rbp + %d]" offset ;
        List.iter
          (fun fieldname ->
            let idx = Hashtbl.find records_idx (typename, fieldname) in
            appfmt buf "mov rsi, [rax + %d]" (idx * 8) ;
            appfmt buf "mov [rdi + %d], rsi" (idx * 8) )
          comp_fieldnames ;
        appstr buf "push rdi" ;
        appfmt buf "/* RecordValueWith %s END */" typename ;
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
              appstr buf @@ gen_raise_exp_of "Stdlib.Match_failure" true ;
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
      appstr buf @@ gen_call_with_aligned_rsp @@ sprintf "%s_detail@PLT" funcname ;
      (* appfmt buf "call %s_detail@PLT" funcname ; *)
      ( match ret_type with
      | CTyInt -> appstr buf @@ tag_int "rax"
      | CTyUnit -> appfmt buf "mov rax, %d" @@ tagged_int 0
      | CTyPtr -> () ) ;
      appstr buf "ret" ; appstr buf ""
    in
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
    appstr buf @@ gen_call_with_aligned_rsp "aqaml_input_char_detail@PLT" ;
    (* appstr buf "call aqaml_input_char_detail@PLT" ; *)
    appstr buf "cmp rax, -1" ;
    appfmt buf "jne %s" exit_label ;
    appstr buf @@ gen_raise_exp_of "Stdlib.End_of_file" false ;
    appfmt buf "%s:" exit_label ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_open_in:" ;
    let exit_label = make_label () in
    appstr buf "mov rdi, rax" ;
    appstr buf @@ gen_call_with_aligned_rsp "aqaml_open_in_detail@PLT" ;
    (* appstr buf "call aqaml_open_in_detail@PLT" ; *)
    appstr buf "cmp rax, 0" ;
    appfmt buf "jne %s" exit_label ;
    (* TODO: raise 'No such file or directory *)
    appstr buf "mov rax, 0" ;
    appstr buf @@ gen_raise_exp_of "Stdlib.Sys_error" true ;
    appfmt buf "%s:" exit_label ;
    appstr buf "ret" ;
    appstr buf "" ;
    appstr buf "aqaml_structural_inequal:" ;
    appstr buf "mov rdi, rax" ;
    appstr buf "mov rsi, rbx" ;
    appstr buf @@ gen_call_with_aligned_rsp "aqaml_structural_equal_detail@PLT" ;
    (* appstr buf "call aqaml_structural_equal_detail@PLT" ; *)
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
    appstr buf @@ gen_call_with_aligned_rsp "aqaml_alloc_block@PLT" ;
    (* appstr buf "call aqaml_alloc_block@PLT" ; *)
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
    appstr buf @@ gen_call_with_aligned_rsp "aqaml_create_string_from_cstr@PLT" ;
    (* appstr buf "call aqaml_create_string_from_cstr@PLT" ; *)
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
    (* set start address of stack *)
    appstr buf ".data" ;
    appstr buf "aqaml_initial_rsp:" ;
    appstr buf ".zero 8" ;
    appstr buf ".text" ;
    appstr buf "mov [rip + aqaml_initial_rsp], rsp" ;
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
