(*
 * (easy) Count the leaves of a binary tree
 *
 * A leaf is a node with no successors. Write a predicate count_leaves/2 to
 * count them. 
 * 
 * % count_leaves(T,N) :- the binary tree T has N leaves 
 *)

use "4.02.sml";

fun countLeaves Empty                    = 0
  | countLeaves (Node (_, Empty, Empty)) = 1
  | countLeaves (Node (_, left, right))  = countLeaves left + countLeaves right
