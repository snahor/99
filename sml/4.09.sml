(*
 * (easy) Collect the leaves of a binary tree in a list
 *
 * A leaf is a node with no successors. Write a predicate leaves/2 to collect
 * them in a list. 
 * 
 * % leaves(T,S) :- S is the list of all leaves of the binary tree T 
 *)

use "4.01.sml";

fun leaves Empty                    = []
  | leaves (Node (x, Empty, Empty)) = [x]
  | leaves (Node (_, left, right))  = leaves left @ leaves right
