(* 
 * (medium) Generate-and-test paradigm
 * 
 * Apply the generate-and-test paradigm to construct all symmetric, completely
 * balanced binary trees with a given number of nodes. Example:
 * ?- sym_cbal_trees(5,Ts).
 * Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x,
 * t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
 * 
 * How many such trees are there with 57 nodes? Investigate about how many
 * solutions there are for a given number of nodes? What if the number is even?
 * Write an appropriate predicate.
 *)

use "4.02.sml"; (* cbalTree *)
use "4.03.sml"; (* symmetric *)

fun symCbalTrees n =
  if n = 0 orelse n mod 2 = 0
  then []
  else List.filter symmetric (cbalTree n)
