
open Printf

(* Back row tell me when you can read this well *)

let max (n : int) (m : int) : int =
  if m > n then
    m
  else
    n
;;

(printf "%d\n" (max 4 10));;
