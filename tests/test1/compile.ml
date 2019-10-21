open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Printf

(*
expr := <number> | '<character>
     |  (let (<name> <expr>) <expr>)
     |  (to-num <expr>) | (to-char <expr>)
     |  (or <expr> <expr>) | (and <expr> <expr>)
     |  (+ <expr> <expr>)
*)
type expr =
  | ENum of int | EChar of char
  | EOr of expr * expr | EAnd of expr * expr
  | EToNum of expr | EToChar of expr
  | EId of string
  | ELet of string * expr * expr
  | EPlus of expr * expr

let int_of_string_opt s =
  try Some(int_of_string s) with Failure _ -> None

let rec sexp_to_expr (se : Sexp.t) : expr =
  match se with
    | Atom("true") -> ENum(1)
    | Atom("false") -> ENum(0)
    | Atom(s) ->
      if ((String.length s) = 2) && ((String.get s 0) = '\'') then
        EChar(String.get s 1)
      else
        (match int_of_string_opt s with
          | None -> EId(s)
          | Some(i) -> ENum(i))
    | List(sexps) ->
      match sexps with
        | [Atom("num"); arg1] -> EToNum(sexp_to_expr arg1)
        | [Atom("chr"); arg1] -> EToChar(sexp_to_expr arg1)
        | [Atom("+"); arg1; arg2] -> EPlus(sexp_to_expr arg1, sexp_to_expr arg2)
        | [Atom("or"); arg1; arg2] -> EOr(sexp_to_expr arg1, sexp_to_expr arg2)
        | [Atom("and"); arg1; arg2] -> EAnd(sexp_to_expr arg1, sexp_to_expr arg2)
        | [Atom("let"); List([Atom(name); e1]); e2] ->
          ELet(name, sexp_to_expr e1, sexp_to_expr e2)
        | _ -> failwith "Parse error"

let parse (s : string) : expr = sexp_to_expr (Sexp.of_string s)

let count = ref 0
let gen_tmp str =
begin count := !count + 1; sprintf "%s%d" str !count end

let stackloc i = (i * 8)
let stackval i = sprintf "[rsp - %d]" (stackloc i)
type tenv = (string * int) list

let rec find (env : tenv) (x : string) : int option =
  match env with
    | [] -> None
    | (y, i)::rest ->
      if y = x then Some(i) else find rest x

let rec e_to_is (e : expr) (si : int) (env : tenv) =
  match e with
    | ENum(i) -> [sprintf "mov rax, %d" ((i * 2) + 1)]
    | EChar(c) -> [sprintf "mov rax, %d" ((Char.code c) * 2)]
    | EToNum(e) ->
      let e_is = e_to_is e si env in
      e_is @ [sprintf "sub rax, 1"]
    | EToChar(e) ->
      let e_is = e_to_is e si env in
      e_is @ [sprintf "add rax, 1"]
    | EPlus(e1, e2) ->
      let e1_is = e_to_is e1 si env in
      let e2_is = e_to_is e2 (si + 1) env in
      let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
      let check = [
        "mov rbx, rax; and rbx, 1"; sprintf "and rbx, [rsp-%d]" (stackloc si);
        "cmp rbx, 0"; "je op_error"
      ] in
      e1_is @ [store_e1] @ e2_is @ check @ [
        sprintf "and rax, 0xFFFFFFFFFFFFFFFE";
        sprintf "add rax, [rsp-%d]" (stackloc si) ]
    | EId(x) ->
      (match find env x with
        | None -> failwith "Unbound id"
        | Some(i) -> [sprintf "mov rax, [rsp - %d]" (stackloc i)])
    | ELet(x, v, body) ->
      let vis = e_to_is v si env in
      let bis = e_to_is body (si + 1) ((x,si)::env) in
      vis @ [sprintf "mov [rsp - %d], rax" (stackloc si)] @ bis
      | EOr(e1, e2) ->
      let fin_lbl = gen_tmp "or_end" in
      let e1_is = e_to_is e1 si env in
      let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
      let restore_e1 = (sprintf "mov rax, [rsp-%d]" (stackloc si)) in
      let e2_is = e_to_is e2 si env in
      e1_is @ [store_e1] @
      ["cmp rax, 1"; sprintf "jne %s" fin_lbl] @ 
      e2_is @
      ["cmp rax, 1"; sprintf "jne %s" fin_lbl] @ 
      [restore_e1; fin_lbl ^ ":"]
    | EAnd(e1, e2) ->
      let fin_lbl = gen_tmp "and_end" in
      let e1_is = e_to_is e1 si env in
      let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
      let e2_is = e_to_is e2 (si + 1) env in
      e1_is @ [store_e1]
      (* YOU WILL FILL THIS IN FOR A QUESTION *)

let compile (program : string) : string =
  let ast = parse program in
  let instrs = e_to_is ast 1 [] in 
  let instrs_str = (String.concat "\n" instrs) in
  sprintf "
section .text
global our_code_starts_here
extern print_err_exit
our_code_starts_here:
  %s
  ret
op_error:
  push 0
  call print_err_exit
  \n" instrs_str

let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let input_program = (input_line input_file) in
  let program = (compile input_program) in
  printf "%s\n" program;;