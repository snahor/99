(*
 * (medium) Determine the prime factors of a given positive integer.
 *
 * Construct a flat list containing the prime factors in ascending order.
 * Example:
 * ?- prime_factors(315, L).
 * L = [3,3,5,7]
 * 
 *)

use "2.01.sml";

(* trial division *)
fun primeFactors n = 
  let
    fun factors_aux n i =
      if i * i > n
      then if n > 1
           then [n]
           else []
      else if isPrime i
           then if n mod i = 0
                then i :: factors_aux (n div i) i
                else factors_aux n (i + 1)
           else factors_aux n (i + 1)
  in
    factors_aux n 2
  end
