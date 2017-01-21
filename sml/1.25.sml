(*
 * (easy) Generate a random permutation of the elements of a list.
 *
 * Example:
 * ?- rnd_permu([a,b,c,d,e,f],L).
 * L = [b,a,d,c,e,f]
 * 
 * Hint: Use the solution of problem 1.23.
 * 
 *)

use "1.23.sml";

fun randomPermutation xs = randomSelect xs (length xs) 
