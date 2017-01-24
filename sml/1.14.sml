(*
 * (easy) Duplicate the elements of a list.
 *
 * Example:
 * ?- dupli([a,b,c,c,d],X).
 * X = [a,a,b,b,c,c,c,c,d,d]
 * 
 *)
fun duplicate xs = foldr (fn (x, acc) => x :: x :: acc) [] xs
