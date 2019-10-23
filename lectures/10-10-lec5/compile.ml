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
  | EBool of bool
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

let true_const = "0xFFFFFFFFFFFFFFFE"
let false_const = "0x7FFFFFFFFFFFFFFE"
let rec e_to_is (e : expr) (si : int) (env : tenv) =
  match e with
    | ENum(i) -> [sprintf "mov rax, %d" ((i * 2) + 1)]
    | EBool(true) -> [sprintf "mov rax, %s" true_const]
    | EBool(false) -> [sprintf "mov rax, %s" false_const]
    | EPlus(e1, e2) ->
      (* Note that this only checks the type on the e1 *)
      let e1_is = e_to_is e1 si env in
      let e2_is = e_to_is e2 (si + 1) env in
      let store_e1 = (sprintf "mov [rsp-%d], rax" (stackloc si)) in
      let check_e1 = [
        "and rax, 1"; "cmp rax, 0"; "je op_error"
      ] in
      (* Note that we check _after_ we store to make sure we store before doing the `and` *)
      e1_is @ [store_e1] @ check_e1 @ e2_is @[
        sprintf "and rax, %s" true_const;
        sprintf "add rax, [rsp-%d]" (stackloc si)
      ]


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
    | EOp(op, e) ->
      let arg_exprs = e_to_is e si env in
      match op with
        | Inc -> arg_exprs @ ["add rax, 2"]
        | Dec -> arg_exprs @ ["sub rax, 2"]


let compile (program : string) : string =
  let ast = parse program in
  let instrs = e_to_is ast 1 [] in 
  let instrs_str = (String.concat "\n" instrs) in
  (* The `push 0` below is to make sure that `rsp` ends up 16-byte aligned.
     It is a total hack that we will improve when we talk about calling
     conventions in more detail. *)
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
