(*
 * (easy) Lotto: Draw N different random numbers from the set 1..M.
 *
 * The selected numbers shall be put into a result list.
 * Example:
 * ?- lotto(6,49,L).
 * L = [23,1,17,33,21,37]
 * 
 * Hint: Combine the solutions of problems 1.22 and 1.23.
 * 
 *)

use "1.22.sml";
use "1.23.sml";

fun lotto n m = randomSelect (range 1 m) n
