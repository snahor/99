(*
 * (medium) Truth tables for logical expressions.
 *
 * Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
 * logical equivalence) which succeed or fail according to the result of their
 * respective operations; e.g. and(A,B) will succeed, if and only if both A and
 * B succeed. Note that A and B can be Prolog goals (not only the constants
 * true and fail).
 * 
 * A logical expression in two variables can then be written in prefix
 * notation, as in the following example: and(or(A,B),nand(A,B)).
 * 
 * Now, write a predicate table/3 which prints the truth table of a given
 * logical expression in two variables.
 * 
 * Example:
 * ?- table(A,B,and(A,or(A,B))).
 * true true true
 * true fail true
 * fail true fail
 * fail fail fail
*)
fun and' (true, true) = true
  | and' (_,    _)    = false

fun or' (false, false) = false
  | or' (_,     _)     = true

(* fun nand' a b = not (and' a b) *)
fun nand' (true, true) = false
  | nand' (_,    _)    = true

(* fun nor' a b = not (or' a b) *)
fun nor' (false, false) = true
  | nor' (_,     _)     = false

  (* fun xor' a b = a = not b *)
fun xor' (true,  false) = true
  | xor' (false, true)  = true
  | xor' (_,     _)     = false

(* fun impl' a b = not (xor' a b) *)
fun impl' (true,  true)  = true
  | impl' (false, false) = true
  | impl' (_,     _)     = false

(* XNOR = NOT XOR *)
fun equ' (true,  true)  = true
  | equ' (false, false) = true
  | equ' (_,     _)     = false

fun fmt true  = "True  "
  | fmt false = "False "

(* this is not a predicate *)
fun table expr =
  let
    fun fmt' p q = fmt p ^ fmt q ^ "| " ^ fmt (expr (p, q)) ^ "\n"
  in
    print (fmt' true true ^ 
           fmt' true false ^
           fmt' false true ^
           fmt' false false)
  end
