(*
 * (easy) A list of prime numbers.
 * Given a range of integers by its lower and upper limit, construct a list of
 * all prime numbers in that range.
 * 
 *)

use "2.01.sml"; 

fun primesRange n m =
  List.filter isPrime (List.tabulate (m - n, fn x => n + x))
