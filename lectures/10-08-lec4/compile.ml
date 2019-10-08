
open Sexplib.Sexp
module Sexp = Sexplib.Sexp


(*
expr := <number>
     |  (<op> <expr>)
     |  (let (<name> <expr>) <expr>)
     |  (+ <expr> <expr>)

op   := inc | dec
*)

type op =
  | Inc
  | Dec

type expr =
  | ENum of int
  | EOp of op * expr
  | EId of string
  | ELet of string * expr * expr
  | EPlus of expr * expr

let int_of_string_opt s =
  try
    Some(int_of_string s)
  with
    Failure _ -> None

let rec sexp_to_expr (se : Sexp.t) : expr =
  match se with
    | Atom(s) ->
      (match int_of_string_opt s with
        | None -> EId(s)
        | Some(i) -> ENum(i))
    | List(sexps) ->
      match sexps with
        | [Atom("inc"); arg] -> EOp(Inc, sexp_to_expr arg)
        | [Atom("dec"); arg] -> EOp(Dec, sexp_to_expr arg)
        | [Atom("+"); arg1; arg2] -> EPlus(sexp_to_expr arg1, sexp_to_expr arg2)
        | [Atom("let"); List([Atom(name); e1]); e2] ->
          ELet(name, sexp_to_expr e1, sexp_to_expr e2)
        | _ -> failwith "Parse error"

let parse (s : string) : expr =
  sexp_to_expr (Sexp.of_string s)

open Printf

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
  (*
    | EPlus(e1, e2) ->
let e1is = e_to_is e1 si env in
let e2is = e_to_is e2 si env in
e1is @
["mov ebx, rax"] @
e2is @
["add rax, ebx"]
    | EPlus(e1, e2) ->
let e1is = e_to_is e1 si env in
let e2is = e_to_is e2 si env in
e1is @
[sprintf "mov %s, rax" stackval] @
e2is @
[sprintf "add %s, rax" (stackval si);
 sprintf "mov rax, %s" (stackval si)]
*)
    | EPlus(e1, e2) ->
let e1is = e_to_is e1 si env in
let e2is = e_to_is e2 (si + 1) env in
e1is @
[sprintf "mov %s, rax" (stackval si)] @
e2is @
[sprintf "mov %s, rax" (stackval (si + 1));
 sprintf "mov rax, %s" (stackval si);
 sprintf "add rax, %s" (stackval (si + 1))]

    | EId(x) ->
      (match find env x with
        | None -> failwith "Unbound id"
        | Some(i) ->
          [sprintf "mov rax, [rsp - %d]" (stackloc i)])
    | ELet(x, v, body) ->
      let vis = e_to_is v si env in
      let bis = e_to_is body (si + 1) ((x,si)::env) in
      vis @
      [sprintf "mov [rsp - %d], rax" (stackloc si)] @
      bis
    | ENum(i) -> [sprintf "mov rax, %d" i]
    | EOp(op, e) ->
      let arg_exprs = e_to_is e si env in
      match op with
        | Inc -> arg_exprs @ ["add rax, 1"]
        | Dec -> arg_exprs @ ["sub rax, 1"]


(* Compiles a source program string to an x86 string *)
let compile (program : string) : string =
  let ast = parse program in
  let instrs = e_to_is ast 1 [] in 
  let instrs_str = (String.concat "\n" instrs) in
  sprintf "
section .text
global our_code_starts_here
our_code_starts_here:
  %s
  ret\n" instrs_str

let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let input_program = (input_line input_file) in
  let program = (compile input_program) in
  printf "%s\n" program;;


