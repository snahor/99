(*
 * (**) Determine whether a given integer number is prime.
 *
 * Example:
 * ?- is_prime(7).
 * Yes
 * 
 *)

fun isPrime n =
  let
    fun loop i =
      if i * i > n
      then true
      else if n mod i = 0 orelse n mod (i + 2) = 0
           then false
           else loop (i + 6)

  in
    if n <= 1
    then false
    else if n < 4
         then true
         else if n mod 2 = 0 orelse n mod 3 = 0
              then false
              else loop 5
  end
