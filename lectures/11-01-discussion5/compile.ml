open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Printf


(*
expr := <number>
     |  (<op> <expr>)
     |  (<name> <expr>)
     |  (if <expr> <expr> <expr>)
     |  (let (<name> <expr>) <expr>)
     |  (+ <expr> <expr>)
     |  (* <expr> <expr>*)
     |  (< <expr> <expr>)
op   := inc | dec
 *)

type op =
  | Inc
  | Dec

type expr =
  | ENum of int
  | EBool of bool
  | EOp of op * expr
  | EId of string
  | EIf of expr * expr * expr
  | ELet of string * expr * expr
  | EPlus of expr * expr
  | ETimes of expr * expr
  | ELess of expr * expr
  | EApp of string * expr * expr

type typ = TNum | TBool

type def =
  | DFun of string * string * typ * string * typ * typ * expr

(* tc_e, tc_p, tc_d *)
          
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
     | [Atom("inc"); arg] -> EOp(Inc, sexp_to_expr arg)
     | [Atom("dec"); arg] -> EOp(Dec, sexp_to_expr arg)
     | [Atom("+"); arg1; arg2] -> EPlus(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("*"); arg1; arg2] -> ETimes(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("<"); arg1; arg2] -> ELess(sexp_to_expr arg1, sexp_to_expr arg2)
     | [Atom("if"); arg1; arg2; arg3] -> EIf(sexp_to_expr arg1, sexp_to_expr arg2, sexp_to_expr arg3)
     | [Atom("let"); List([Atom(name); e1]); e2] ->
        ELet(name, sexp_to_expr e1, sexp_to_expr e2)
     | [Atom(fname); arg1; arg2] ->
        EApp(fname, sexp_to_expr arg1, sexp_to_expr arg2)
     | _ -> failwith ("Parse error: " ^ (Sexp.to_string se))

(* 
(def (f x : Num) : Bool
  (< x 10))
 *)
let parse_typ t =
  match t with
  | "Num" -> TNum
  | "Bool" -> TBool
  | _ -> failwith "Invalid type"

let parse_def sexp =
  match sexp with
  | List([Atom("def");
          (List([Atom(name); Atom(argname1);Atom(":");Atom(arg_typ1);
                 Atom(argname2);Atom(":");Atom(arg_typ2)]));
          Atom(":");Atom(ret_typ);
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
let true_const = "2"
let false_const = "0"

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
  | ETimes(e1, e2) ->
     let e1_is = e_to_is e1 si env false in
     let e2_is = e_to_is e2 (si + 1) env false in
     let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
     e1_is @ [store_e1] @ e2_is @[
         "sub rax, 1";
         sprintf "mov [rsp-%d], rax" (stackloc (si + 1));
         sprintf "mov rax, [rsp-%d]" (stackloc si);
         "sar rax, 1";
         sprintf "imul rax, [rsp-%d]" (stackloc (si + 1));
         "add rax, 1";
       ]
  | ELess(e1, e2) ->
     let e1_is = e_to_is e1 si env false in
     let e2_is = e_to_is e2 (si + 1) env false in
     let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
     e1_is @ [store_e1] @ e2_is @[
         sprintf "sub rax, [rsp-%d]" (stackloc si);
         "sub rax, 1";
         "shr rax, 62";
         "xor rax, 2";
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
  | EOp(op, e) ->
     let arg_exprs = e_to_is e si env false in
     begin match op with
     | Inc -> arg_exprs @ ["add rax, 2"]
     | Dec -> arg_exprs @ ["sub rax, 2"]
     end
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
  | EApp(f, arg1, arg2) ->
      match find_def defs f with
        | None -> failwith ("No such function definition " ^ f)
        | Some(DFun(name, param, _, _, _, _, body)) ->
          if is_tail_call then
          let arg1_is = e_to_is arg1 si env false in
          let arg2_is = e_to_is arg2 (si + 1) env false in
          (* We don't need to create new argument space or a return pointer
          let stack_index_above_all_arg_space = si + 2 in
          let after = gen_temp "after_call" in *)
          let save_arg1 = sprintf "mov [rsp-%d], rax" (stackloc si) in
          let save_arg2 = sprintf "mov [rsp-%d], rax" (stackloc (si + 1)) in
          arg1_is @ [save_arg1] @
          arg2_is @ [save_arg2] @ [
            (* No need to save return ptr, old rsp, we will use the existing ones
            sprintf "mov rbx, %s" after;
            sprintf "mov [rsp-%d], rbx" (stackloc stack_index_above_all_arg_space);
            sprintf "mov [rsp-%d], rsp" (stackloc (stack_index_above_all_arg_space + 1));
            *)
            sprintf "; start of tail call to function %s" f;
            sprintf "mov rax, [rsp-%d]" (stackloc si);
            sprintf "mov [rsp-%d], rax ; overwrite arg1 with new arg1" (stackloc 2); (* These are now constants matching parameter positions *)
            sprintf "mov rax, [rsp-%d]" (stackloc (si + 1));
            sprintf "mov [rsp-%d], rax ; overwrite arg2 with new arg2" (stackloc 3);
            (* Leave RSP where it is
            sprintf "sub rsp, %d" (stackloc stack_index_above_all_arg_space);  *)
            sprintf "jmp %s" name; (* In tail recursive cases, this will jump to the beginning of the function, turning the body into a loop! *)
            (* there is nothing to do after the call!
              sprintf "%s:" after;
              sprintf "mov rsp, [rsp-16]"; *)
          ]
          else
          let arg1_is = e_to_is arg1 si env false in
          let arg2_is = e_to_is arg2 (si + 1) env false in
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
            sprintf "mov rax, [rsp-%d]" (stackloc si);
            sprintf "mov [rsp-%d], rax" (stackloc (stack_index_above_all_arg_space + 2));
            sprintf "mov rax, [rsp-%d]" (stackloc (si + 1));
            sprintf "mov [rsp-%d], rax" (stackloc (stack_index_above_all_arg_space + 3));
            sprintf "sub rsp, %d" (stackloc stack_index_above_all_arg_space);
            sprintf "jmp %s" name;
            sprintf "%s:" after;
            sprintf "mov rsp, [rsp-16]";
          ]

let compile_def (DFun(name, param1, _, param2, _, _, body)) defs =
  let body_is = e_to_is body 4 [(param1, 2); (param2, 3)] true defs in
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
           extern print_err_exit
           %s
           our_code_starts_here:
           %s
           ret

           op_error:
           push 0
           call print_err_exit
           \n" defs_str instrs_str

let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let result = compile (Sexplib.Sexp.input_sexps input_file) in
  printf "%s\n" result;;
