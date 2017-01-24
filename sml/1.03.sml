(* 
 * (easy) Find the K'th element of a list.
 *
 * The first element in the list is number 1.
 * Example:
 * ?- element_at(X,[a,b,c,d,e],3).
 * X = c
 * 
 *) 
exception NotFound

fun kth_element ([], _)      = raise NotFound
  | kth_element (x :: xs, k) =
      if k = 1
      then x
      else kth_element (xs, k - 1)
