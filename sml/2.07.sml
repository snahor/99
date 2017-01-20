(*
 * (medium) Determine the greatest common divisor of two positive integer
 * numbers.
 *
 * Use Euclid's algorithm.
 * Example:
 * ?- gcd(36, 63, G).
 * G = 9
 * 
 * Define gcd as an arithmetic function; so you can use it like this:
 * ?- G is gcd(36,63).
 * G = 9
 * 
 *)

fun gcd a 0 = a
  | gcd a b = gcd b (a mod b)
