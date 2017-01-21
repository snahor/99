(*
 * (easy) Insert an element at a given position into a list.
 *
 * Example:
 * ?- insert_at(alfa,[a,b,c,d],2,L).
 * L = [a,alfa,b,c,d]
 * 
 *)

(* 1-indexed *)
fun insertAt x []        _ = [x]
  | insertAt x ws        1 = x :: ws
  | insertAt x (w :: ws) n = w :: insertAt x ws (n - 1)

(*
 * insertAt 666 [] 1 = [666]
 * insertAt 666 [1] 2 = [1,666]
 * insertAt 666 [1] 22 = [1,666]
 * insertAt 666 [] 2 = [666]
 *
 *)

