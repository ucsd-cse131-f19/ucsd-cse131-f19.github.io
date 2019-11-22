open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Printf


type expr =
  | ENum of int
  | EBool of bool
  | EId of string
  | EIf of expr * expr * expr
  | ELet of string * expr * expr
  | EPlus of expr * expr
  | EMinus of expr * expr
  | ELess of expr * expr
  | EEq of expr * expr
  | EApp of string * expr * expr
  | EPair of expr * expr
  | EFst of expr | ESetFst of expr * expr
  | ESnd of expr | ESetSnd of expr * expr

and typ = TNum | TBool | TPair of typ * typ

type def =
  | DFun of string * string * typ * string * typ * typ * expr

type prog = def list * expr
          
let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let int_of_string_opt s =
  try
    Some(int_of_string s)
  with
    Failure _ -> None

let rec sexp_to_expr (se : Sexp.t) : expr =
  match se with
  | Atom("true") -> EBool(true)
  | Atom("false") -> EBool(false)
  | Atom(s) ->
     (match int_of_string_opt s with
      | None -> EId(s)
      | Some(i) -> ENum(i))
  | List(sexps) ->
     match sexps with
     | [Atom("+"); arg1; arg2] -> EPlus(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("-"); arg1; arg2] -> EMinus(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("<"); arg1; arg2] -> ELess(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("="); arg1; arg2] -> EEq(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("pair"); arg1; arg2] -> EPair(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("first"); arg1] -> EFst(sexp_to_expr arg1)
     | [Atom("second"); arg1] -> ESnd(sexp_to_expr arg1)
     | [Atom("set-first"); arg1; arg2] -> ESetFst(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("set-second"); arg1; arg2] -> ESetSnd(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("if"); arg1; arg2; arg3] -> EIf(sexp_to_expr arg1, sexp_to_expr arg2, sexp_to_expr arg3)
     | [Atom("let"); List([Atom(name); e1]); e2] ->
        ELet(name, sexp_to_expr e1, sexp_to_expr e2)
     | [Atom(fname); arg1; arg2] ->
        EApp(fname, sexp_to_expr arg1, sexp_to_expr arg2)
     | _ -> failwith ("Parse error: " ^ (Sexp.to_string se))

let rec parse_typ t =
  match t with
  | Atom("Num") -> TNum
  | Atom("Bool") -> TBool
  | List([t1; Atom("*"); t2]) -> TPair(parse_typ t1, parse_typ t2)
  | _ -> failwith "Invalid type"

let parse_def sexp =
  match sexp with
  | List([Atom("def");
          (List([Atom(name); Atom(argname1);Atom(":");arg_typ1;
                 Atom(argname2);Atom(":");arg_typ2]));
          Atom(":");ret_typ;
          body]) ->
     DFun(name, argname1, parse_typ arg_typ1, argname2, parse_typ arg_typ2, parse_typ ret_typ, sexp_to_expr body)
  | _ -> failwith ("Invalid sexp: def " ^ (Sexp.to_string sexp))

let rec parse_program (sexps : Sexp.t list) : prog =
  match sexps with
  | [] -> failwith "Empty program"
  | [e] -> ([], sexp_to_expr e)
  | def::es ->
     let defs, body = (parse_program es) in
     ((parse_def def)::defs, body)

let parse (s : string) : prog =
  match (Sexp.of_string ("(" ^ s ^")")) with
  | Atom(_) -> failwith "Impossible"
  | List(es) -> parse_program es

let stackloc i = (i * 8)
let stackval i = sprintf "[rsp - %d]" (stackloc i)
type tenv = (string * int) list

let rec find env (x : string) =
  match env with
  | [] -> None
  | (y, i)::rest ->
     if y = x then Some(i) else find rest x

let rec find_def (defs : def list) (x : string) : def option =
  match defs with
  | [] -> None
  | (DFun(fname, _, _, _, _, _, _) as d)::rest ->
     if fname = x then Some(d) else find_def rest x

let tag_mask = "0xFFFFFFFFFFFFFFFE"
let true_const = "6"
let false_const = "2"

let rec e_to_is (e : expr) (si : int) (env : tenv) (is_tail_call : bool) (defs : def list) =
  let e_to_is e si env is_tail_call  = e_to_is e si env is_tail_call defs in
  match e with
  | ENum(i) -> [sprintf "mov rax, %d" ((i * 2) + 1)]
  | EBool(true) -> [sprintf "mov rax, %s" true_const]
  | EBool(false) -> [sprintf "mov rax, %s" false_const]
  | EPlus(e1, e2) ->
     let e1_is = e_to_is e1 si env false in
     let e2_is = e_to_is e2 (si + 1) env false in
     let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
     e1_is @ [store_e1] @ e2_is @[
         sprintf "and rax, %s" tag_mask;
         sprintf "add rax, [rsp-%d]" (stackloc si)
       ]
  | EMinus(e1, e2) ->
     let e1_is = e_to_is e1 si env false in
     let e2_is = e_to_is e2 (si + 1) env false in
     let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
     e1_is @ [store_e1] @ e2_is @[
         sprintf "mov [rsp-%d], rax" (stackloc (si + 1));
         sprintf "mov rax, [rsp-%d]" (stackloc si);
         sprintf "sub rax, [rsp-%d]" (stackloc (si + 1));
         "add rax, 1";
       ]
  | EEq(e1, e2) ->
     let e1_is = e_to_is e1 si env false in
     let e2_is = e_to_is e2 (si + 1) env false in
     let true_lbl = gen_temp "true" in
     let end_lbl = gen_temp "end" in
     let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
     e1_is @ [store_e1] @ e2_is @[
         sprintf "cmp rax, [rsp-%d]" (stackloc si);
         sprintf "je %s" true_lbl;
         sprintf "mov rax, %s" false_const;
         sprintf "jmp %s" end_lbl;
         sprintf "%s:" true_lbl;
         sprintf "mov rax, %s" true_const;
         sprintf "%s:" end_lbl;
       ]
  | ELess(e1, e2) ->
     let e1_is = e_to_is e1 si env false in
     let e2_is = e_to_is e2 (si + 1) env false in
     let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
     e1_is @ [store_e1] @ e2_is @[
         sprintf "sub rax, [rsp-%d]" (stackloc si);
         "sub rax, 1";
         "shr rax, 62";
         "xor rax, 6";
         sprintf "and rax, %s" tag_mask;
       ]
  | EId(x) ->
     (match find env x with
      | None -> failwith "Unbound id"
      | Some(i) ->
         [sprintf "mov rax, [rsp - %d]" (stackloc i)])
  | ELet(x, v, body) ->
     let vis = e_to_is v si env false in
     let bis = e_to_is body (si + 1) ((x,si)::env) is_tail_call in
     vis @
       [sprintf "mov [rsp - %d], rax" (stackloc si)] @
         bis
  | EIf(ifexpr,thenexpr,elseexpr) ->
     let ifexpr = e_to_is ifexpr si env false in
     let thenexpr = e_to_is thenexpr si env is_tail_call in
     let elseexpr = e_to_is elseexpr si env is_tail_call in
     let else_lbl = gen_temp "else" in
     let end_lbl = gen_temp "end_if" in
     ifexpr @
       [ sprintf "cmp rax, %s" true_const;
         sprintf "jne %s" else_lbl;
       ] @
         thenexpr @
           [ sprintf "jmp %s" end_lbl;
             sprintf "%s:" else_lbl;
           ] @
             elseexpr @
               [sprintf "%s:" end_lbl]
  | EPair(e1, e2) ->
    let actually_alloc = gen_temp "alloc" in
    let check_mem = [
      "mov rax, HEAP_END";
      "mov rax, [rax]";
      "sub rax, 24";
      "cmp rax, r15";
      (* sprintf "jge %s" actually_alloc; *)
      sprintf "mov [rsp-%d], rsp" (stackloc (si + 2));
      sprintf "sub rsp, %d" (stackloc (si + 2));
      "call gc";
      sprintf "add rsp, %d" (stackloc (si + 2));
      sprintf "%s:" actually_alloc
    ] in
    let e1_is = e_to_is e1 si env false in
    let e2_is = e_to_is e2 (si + 1) env false in
    let save_e1 = sprintf "mov [rsp-%d], rax" (stackloc si) in
    let save_e2 = sprintf "mov [rsp-%d], rax" (stackloc (si + 1)) in
    e1_is @ [save_e1] @ e2_is @ [save_e2] @ check_mem @ [
      sprintf "mov rax, [rsp-%d]" (stackloc si);
      sprintf "mov [r15 + 8], rax";
      sprintf "mov rax, [rsp-%d]" (stackloc (si + 1));
      sprintf "mov [r15 + 16], rax";
      sprintf "mov rax, r15";
      sprintf "add r15, 24";
    ]
  | EFst(e) ->
    let e_is = e_to_is e si env false in
    e_is @ [sprintf "mov rax, [rax + 8]"]
  | ESnd(e) ->
    let e_is = e_to_is e si env false in
    e_is @ [sprintf "mov rax, [rax + 16]"]
  | ESetFst(e_pair, e_val) ->
    let e1_is = e_to_is e_pair (si + 1) env false in
    let e2_is = e_to_is e_val si env false in
    let save_e1 = sprintf "mov [rsp-%d], rax" (stackloc si) in
    e1_is @ [save_e1] @ e2_is @ [
      sprintf "mov rbx, [rsp-%d]" (stackloc si);
      sprintf "mov [rbx + 8], rax"]
  | ESetSnd(e_pair, e_val) ->
    let e1_is = e_to_is e_pair (si + 1) env false in
    let e2_is = e_to_is e_val si env false in
    let save_e1 = sprintf "mov [rsp-%d], rax" (stackloc si) in
    e1_is @ [save_e1] @ e2_is @ [
      sprintf "mov rbx, [rsp-%d]" (stackloc si);
      sprintf "mov [rbx + 16], rax"]
  | EApp(f, arg1, arg2) ->
      match find_def defs f with
        | None -> failwith ("No such function definition " ^ f)
        | Some(DFun(name, param, _, _, _, _, body)) ->
          let arg1_is = e_to_is arg1 si env false in
          let arg2_is = e_to_is arg2 (si + 1) env false in
          if is_tail_call then
          let save_arg1 = sprintf "mov [rsp-%d], rax" (stackloc si) in
          let save_arg2 = sprintf "mov [rsp-%d], rax" (stackloc (si + 1)) in
          arg1_is @ [save_arg1] @
          arg2_is @ [save_arg2] @ [
            sprintf "; start of tail call to function %s" f;
            sprintf "mov rax, [rsp-%d]" (stackloc si);
            sprintf "mov [rsp-%d], rax ; overwrite arg1 with new arg1" (stackloc (-2));
            sprintf "mov rax, [rsp-%d]" (stackloc (si + 1));
            sprintf "mov [rsp-%d], rax ; overwrite arg2 with new arg2" (stackloc (-1));
            sprintf "jmp %s" name; 
          ]
          else
          let stack_index_above_all_arg_space = si + 2 in
          let after = gen_temp "after_call" in
          let save_arg1 = sprintf "mov [rsp-%d], rax" (stackloc si) in
          let save_arg2 = sprintf "mov [rsp-%d], rax" (stackloc (si + 1)) in
          arg1_is @ [save_arg1] @
          arg2_is @ [save_arg2] @ [
            sprintf "; start of regular call to function %s" f;
            sprintf "mov rbx, %s" after;
            sprintf "mov [rsp-%d], rbx" (stackloc stack_index_above_all_arg_space);
            sprintf "mov [rsp-%d], rsp" (stackloc (stack_index_above_all_arg_space + 1));
            sprintf "sub rsp, %d" (stackloc stack_index_above_all_arg_space);
            sprintf "jmp %s" name;
            sprintf "%s:" after;
            sprintf "mov rsp, [rsp-16]";
          ]


let compile_def (DFun(name, param1, _, param2, _, _, body)) defs =
  let body_is = e_to_is body 2 [(param1, -2); (param2, -1)] true defs in
  [
    sprintf "%s:" name;
  ] @ body_is @ [
      "ret"
    ]

let compile (program : Sexp.t list) : string =
  let (defs, body) = parse_program program in
  let defs_instrs = List.concat (List.map (fun d -> compile_def d defs) defs) in
  let instrs = e_to_is body 1 [] false defs in
  let defs_str = (String.concat "\n" defs_instrs) in
  let instrs_str = (String.concat "\n" instrs) in
  sprintf "
           section .text
           global our_code_starts_here
           extern HEAP_END
           extern gc
           extern print_err_exit
           %s
           our_code_starts_here:
           mov r15, rdi
           %s
           ; mov rax, HEAP_END
           ; mov rax, [rax]
           ret

           oom_error:
            push 0
            call print_err_exit

           \n" defs_str instrs_str

let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let result = compile (Sexplib.Sexp.input_sexps input_file) in
  printf "%s\n" result;;
