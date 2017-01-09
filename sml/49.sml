(*
 * Gray code. (medium)
 * 
 * An n-bit Gray code is a sequence of n-bit strings constructed according to
 * certain rules. For example,
 * n = 1: C(1) = ['0','1'].
 * n = 2: C(2) = ['00','01','11','10'].
 * n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
 * 
 * Find out the construction rules and write a predicate with the following
 * specification:
 * 
 * % gray(N,C) :- C is the N-bit Gray code
 *)

(* wikipedia *)
fun gray 0 = []
  | gray 1 = ["0", "1"]
  | gray n =
      let
        val xs = gray (n - 1)
      in
        (map (fn x => "0" ^ x) xs) @ (map (fn x => "1" ^ x) (rev xs))
      end
