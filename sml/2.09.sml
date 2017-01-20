(*
 * (medium) Calculate Euler's totient function phi(m).
 *
 * Euler's so-called totient function phi(m) is defined as the number of
 * positive integers r (1 <= r < m) that are coprime to m.
 * 
 * Example: m = 10: r = 1,3,7,9; thus phi(m) = 4.
 * Note the special case: phi(1) = 1.
 * 
 * ?- Phi is totient_phi(10).
 * Phi = 4
 * 
 * Find out what the value of phi(m) is if m is a prime number. Euler's totient
 * function plays an important role in one of the most widely used public key
 * cryptography methods (RSA). In this exercise you should use the most
 * primitive method to calculate this function. There is a smarter way that we
 * shall use in 2.10.
 * 
 *)
use "2.08.sml";

(* https://en.wikipedia.org/wiki/Euler's_totient_function#Divisor_sum *)
local
  open List;
in
  fun coprimes n = filter (fn x => coprime n x) (tabulate (n, fn x => x + 1))

  fun phi n =
    if n < 1
    then 0
    else length (coprimes n)
end
