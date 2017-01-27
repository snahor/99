(*
 * (easy) Collect the nodes at a given level in a list
 * 
 * A node of a binary tree is at level N if the path from the root to the node
 * has length N-1. The root node is at level 1. Write a predicate atlevel/3 to
 * collect all nodes at a given level in a list. 
 * 
 * % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
 * 
 * Using atlevel/3 it is easy to construct a predicate levelorder/2 which
 * creates the level-order sequence of the nodes. However, there are more
 * efficient ways to do that. 
 *)

fun nodesAtLevel (Node (x, _, _)) 1    = [x]
  | nodesAtLevel Empty            _    = []
  | nodesAtLevel (Node (_, lt, rt)) n  =
      nodesAtLevel lt (n - 1) @ nodesAtLevel rt (n - 1)
