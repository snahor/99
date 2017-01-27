(*
 * (easy) Collect the internal nodes of a binary tree in a list
 * 
 * An internal node of a binary tree has either one or two non-empty
 * successors. Write a predicate internals/2 to collect them in a list. 
 * 
 * % internals(T,S) :- S is the list of internal nodes of the binary tree T. 
 *)

fun internals Empty                    = []
  | internals (Node (_, Empty, Empty)) = []
  | internals (Node (x, left, right))  = x :: internals left @ internals right
