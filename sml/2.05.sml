(*
 * (medium) Goldbach's conjecture.
 *
 * Goldbach's conjecture says that every positive even number greater than 2 is
 * the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
 * famous facts in number theory that has not been proved to be correct in the
 * general case. It has been numerically confirmed up to very large numbers
 * (much larger than we can go with our Prolog system). Write a predicate to
 * find the two prime numbers that sum up to a given even integer.
 * 
 * Example:
 * ?- goldbach(28, L).
 * L = [5,23]
 * 
 *)

use "2.01.sml";

fun goldbach n =
  let
    fun aux i =
      if i > n
      then (0, 0)
      else if isPrime i andalso isPrime (n - i)
           then (i, n - i)
           else aux (i + 1)
  in
    aux 2
  end
