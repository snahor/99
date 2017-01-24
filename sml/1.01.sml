(* 
 * (easy) Find the last element of a list.
 *
 * Example:
 * ?- my_last(X,[a,b,c,d]).
 * X = d
 *
 *) 
fun last (x :: nil) = x
  | last (x :: xs)  = last xs
  | last []         = raise List.Empty

fun last' xs = hd (List.drop (xs, length xs - 1))
  | last' [] = raise List.Empty
