open Printf

type expr =
  | IntNode of int (* IntNode is a constructor or variant *)
  | ExprNode of string * expr * expr
;;

let e1 = ExprNode("+", IntNode(1), IntNode(1));;
let e2 = IntNode(3);;

let op_of_e1 = match e1 with
  | ExprNode(op, lhs, rhs) -> op
  | IntNode(value) -> "has to be a string"
;;

let rec eval (e : expr) : int =
  match e with
    | IntNode(value) -> value
    | ExprNode("+", lhs, rhs) -> (eval lhs) + (eval rhs)
    | ExprNode("-", lhs, rhs) -> (eval lhs) - (eval rhs)
    | ExprNode(_, lhs, rhs) -> failwith "Unknown operator"
      if op = "+" then
      else if op = "-" then
      else
        failwith "Unknown operator"
;;

(printf "%d\n" (eval e1));
(printf "%d\n" (eval e2));
(printf "%d\n" (eval (ExprNode("bogus", IntNode(1), IntNode(1)))));
(printf "%s\n" op_of_e1)

(*

How would you represent arithmetic expressions?

- A tree, where nodes are operators and edges point to other
  expressions (which could also be trees)

- Java:

interface Expr { ... }
class IntNode implements Expr {
  int value;
  ...
}
class ExprNode implements Expr {
  String op;
  Expr lhs, rhs;
  public ExprNode(String op, Expr lhs, Expr rhs) { ... }
}
new ExprNode("+", new IntNode(5), new IntNode(6))








class Node {
  String op;
  boolean amIANumberOrAmIAnExpression;
  Node lhs, rhs;
  int value;
  public Node(String op, Node lhs, Node rhs) { ... }
  public Node(String op, int value) { ... }
}
new Node("+", 1, 1)

- Maybe use unions in C

- Enum written in Rust, either a int or an operator that has two
  expressions

enum expr { 

(Recursion will be a useful tool)


*)


(* 1 + 1 *)
(*

1 + (2 * 3)

*)

(* 2 * 2 *)

(* int max(int n, int m) *)

(*
let max (n : int) (m : int) : int =
  if n > m then
    n
  else
    m
;;

(printf "%d\n" (max 47 10))
*)





