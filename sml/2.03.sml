(*
 * (medium) Determine the prime factors of a given positive integer (2).
 * Construct a list containing the prime factors and their multiplicity.
 * Example:
 * ?- prime_factors_mult(315, L).
 * L = [[3,2],[5,1],[7,1]]
 * 
 * Hint: The solution of problem 1.10 may be helpful.
 * 
 *)

use "2.02.sml";

fun group xs =
  let
    fun aux (x, [])           = [(x, 1)]
      | aux (x, (w, n) :: ws) =
          if x = w
          then (w, n + 1) :: ws
          else (x, 1) :: (w, n) :: ws
  in
    foldr aux [] xs
  end

val primeFactorsMult = group o primeFactors
