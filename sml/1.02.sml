(* 
 * (easy) Find the last but one element of a list.
 *
 * (de: zweitletztes Element, fr: avant-dernier élément)
 * 
 *) 
fun lastButOne (x :: y :: nil) = x
  | lastButOne (x :: xs)       = lastButOne xs
  | lastButOne []              = raise Empty
