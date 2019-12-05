#load "str.cma";;
open Printf
open Str

(* This file has two implementations of parsing for s-expressions. They both
use the same tokenizer. *)

(* The end goal is to produce s_exps from strings *)
type s_exp =
  | SNum of int
  | SName of string
  | SList of s_exp list

(* These are useful for debugging but don't help us write the parser *)
let rec str_of_toks toks =
  match toks with
    | [] -> ""
    | (tok_type, str)::rest -> (sprintf "(%s, \"%s\")" tok_type str) ^ "; " ^ (str_of_toks rest);;
let rec str_of_expr e =
  match e with
    | SName(n) -> sprintf "SName(%s)" n
    | SNum(n) -> sprintf "SNum(%d)" n
    | SList(exprs) -> "SList(" ^ (String.concat "," (List.map str_of_expr exprs)) ^ ")"


(* These types will help us make things simpler to write down later *)
type token = (string * string)
type 'a parsed = ('a option * token list)
type 'a parser = token list -> 'a parsed


let pats = [
  (regexp "[0-9]+", fun str -> ("num", str));
  (regexp "[a-zA-Z][a-zA-Z0-9]*", fun str -> ("name", str));
  (regexp "(", fun str -> ("LPAREN", str));
  (regexp ")", fun str -> ("RPAREN", str));
  (regexp "[ \n\t\r]*", fun str -> ("WS", str));
]

let rec tok str start pats : (string * string) list =
  if String.length str = start then []
  else
    let rec first_match pats =
      match pats with
        | [] -> failwith (sprintf "Tokenizer error at character %d" start)
        | (reg, f)::restpats ->
          if string_match reg str start then
            f (matched_string str)
          else
            first_match restpats
    in
    let (tok_type, content) = first_match pats in
    (tok_type, content)::(tok str (start + (String.length content)) pats);;

(* This is the parser from lecture, but with the fully general list case for
parenthesized sequences *)
let rec parse_expr toks : (s_exp option * (string * string) list) =
  begin
    let ans = match toks with
      | [] -> failwith "Empty program?"
      | ("WS", _)::rest -> parse_expr rest
      | ("num", n)::rest -> (Some(SNum(int_of_string n)), rest)         (* <expr> := <number>                 *)
      | ("name", n)::rest -> (Some(SName(n)), rest)                     (*        |  <name>                   *)
      | ("LPAREN", _)::rest ->                                          (*        |  LPAREN                   *)
        (match parse_list rest with                                     (*            <expr list>             *)
          | Some(exprs), ("RPAREN", _)::rest ->                         (*           RPAREN                   *)
          (* The line above has the bug!!! This doesn't handle the case where
          there is whitespace before the right paren. See below for an idea –
          filtering out whitespace first *)
            Some(SList(exprs)), rest
          | _, remaining -> None, remaining)
      | _ -> None, toks
    in
    match ans with
      | Some(e), _ ->
        begin printf "Producing: %s\n" (str_of_expr e); ans end
      | None, _ -> ans
  end
and parse_list toks : (s_exp list option * (string * string) list) =
  let (first_expr, remaining) = parse_expr toks in
  match first_expr with
    | None -> (Some([]), toks)
    | Some(first_expr) ->
      let (rest_list, remaining_after) = parse_list remaining in        (* <expr list> := <expr>             *)
      (match rest_list with                                             (*             |  <expr> <expr list> *)
        | Some(rest_list) ->
          (Some(first_expr::rest_list), remaining_after)
        | None -> None, remaining_after)
let parse (toks : (string * string) list) : s_exp =
  match parse_expr toks with (* This is where we could filter out whitespace before processing *)
    | Some(e), [] -> e
    | Some(e), lst -> failwith (sprintf "Extra tokens at end: %s" (str_of_toks lst))
    | None, lst -> failwith (sprintf "Parse error, remaining toks were: %s" (str_of_toks lst))


  (* There's a different strategy called *parser combinators* that we could
  use here as well. It involves defining functions for sequencing, choice,
  and so on, and using those to construct a “grammar” out of functions *)


(* Sequence is like two grammar productions after one another *)
let rec sequence (g1 : 'a parser) (g2 : 'b parser) toks : ('a * 'b) parsed =
  match g1 toks with
    | (Some(first), remaining) ->
      begin match g2 remaining with
      | (Some(second), remaining) ->
        Some(first, second), remaining
      | _ -> None, toks
      end
    | _ -> None, toks

(* Many is like ... or *; a sequence of many of the same production *)
let rec many (g : 'a parser) toks : 'a list parsed =
  match g toks with
    | (Some(first), remaining) ->
      begin match many g remaining with
        | (Some(rest), remaining) -> Some(first :: rest), remaining
        | (None, remaining) -> Some([first]), remaining
      end
    | _ -> None, toks

(* Either is like | (pipe) for choice. Note that it is ordered choice! If g1
succeeds we never try g2 *)
let either (g1 : 'a parser) (g2 : 'a parser) toks : 'a parsed =
  match g1 toks with
    | Some(g1result), remaining -> Some(g1result), remaining
    | _ ->
      match g2 toks with
        | Some(g2result), remaining -> Some(g2result), remaining
        | _ -> None, toks

let literal typ create toks =
    match toks with
      | (toktyp, v)::rest ->
        if toktyp = typ then Some(create v), rest else None, rest
      | _ -> None, toks


let lparen = literal "LPAREN" (fun _ -> "(")
let rparen = literal "RPAREN" (fun _ -> ")")
let name = literal "name" (fun n -> SName(n))
let num = literal "num" (fun n -> SNum(int_of_string n))

(* parenexp := (<expr> ...) *)
let rec parenexp toks : s_exp parsed = 
  match (sequence (sequence lparen (many sexp)) rparen) toks with
    | Some((_, exprs), _), remaining -> (Some(SList(exprs)), remaining)
    | _ -> None, toks
(* expr := <number> | <name> | <parenexp> *)
and sexp toks : s_exp parsed = (either (either parenexp name) num) toks

let parse2 (toks : (string * string) list) : s_exp =
  match sexp (List.filter (fun (t, _) -> not(t = "WS")) toks) with
    | Some(e), [] -> e
    | Some(e), lst -> failwith (sprintf "Extra tokens at end: %s" (str_of_toks lst))
    | None, lst -> failwith (sprintf "Parse error, remaining toks were: %s" (str_of_toks lst))

let () =
  begin
    printf "%s\n" (str_of_toks (tok "(5 6 xyz (11 30))" 0 pats));
    printf "%s\n" (str_of_expr (parse2 (tok "(5 6 xyz (11 30))" 0 pats)));
  end
