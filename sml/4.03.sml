(*
 * (medium) Symmetric binary trees
 * 
 * Let us call a binary tree symmetric if you can draw a vertical line through
 * the root node and then the right subtree is the mirror image of the left
 * subtree. Write a predicate symmetric/1 to check whether a given binary tree
 * is symmetric. Hint: Write a predicate mirror/2 first to check whether one
 * tree is the mirror image of another. We are only interested in the
 * structure, not in the contents of the nodes.
 *)

use "4.01.sml"; (* bintree definition *)

fun mirror Empty            Empty            = true
  | mirror (Node (_, a, b)) (Node (_, c, d)) = mirror a d andalso mirror b c
  | mirror _                _                = false

fun symmetric t = mirror t t
