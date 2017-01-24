(*
 * (medium) Truth tables for logical expressions (3).
 *
 * Generalize problem 3.02 in such a way that the logical expression may
 * contain any number of logical variables. Define table/2 in a way that
 * table(List,Expr) prints the truth table for the expression Expr, which
 * contains the logical variables enumerated in List.
 *
 * Example:
 * ?- table([A,B,C], A and (B or C) equ A and B or A and C).
 * true true true true
 * true true fail true
 * true fail true true
 * true fail fail true
 * fail true true true
 * fail true fail true
 * fail fail true true
 * fail fail fail true
 *)

use "3.01.sml";
use "utils.sml";

(* taken from http://gallium.inria.fr/blog/on-the-nary-cartesian-product/ *)
fun n_cartesian_product []       = [[]]
  | n_cartesian_product (h :: t) =
    let
      val rest = n_cartesian_product t
    in
      List.concat (map (fn i => map (fn r => i :: r) rest) h)
    end

fun repeat n x = List.tabulate (n, fn _ => x)

(* haskell's replicateM implementation*)
fun replicateM n xs = n_cartesian_product (repeat n xs)

fun truthVars numVars =
  let
    fun charToBool #"0" = false
      | charToBool _    = true

    fun bitsToBoolean bits =
      let
        val size = String.size bits
        val booleans = map charToBool (String.explode bits)
      in
        List.tabulate (numVars - size, fn _ => false) @ booleans
      end
    
    fun aux n =
      List.tabulate (n, fn x => bitsToBoolean (Word.fmt StringCvt.BIN (Word.fromInt (n - x - 1))))
  in
    aux (2 ** numVars)
  end

fun table' numVars expr =
  let
    fun fmt' xs = concat (map fmt xs) ^ "| " ^ fmt (expr xs) ^ "\n"
  in
    print (concat (map fmt' (truthVars numVars)))
  end
