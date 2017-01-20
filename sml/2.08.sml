(*
 * (easy) Determine whether two positive integer numbers are coprime.
 *
 * Two numbers are coprime if their greatest common divisor equals 1.
 * Example:
 * ?- coprime(35, 64).
 * Yes
 * 
 *)
use "2.07.sml";

fun coprime a b = gcd a b = 1
