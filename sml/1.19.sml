(*
 * (medium) Rotate a list N places to the left.
 *
 * Examples:
 * ?- rotate([a,b,c,d,e,f,g,h],3,X).
 * X = [d,e,f,g,h,a,b,c]
 * 
 * ?- rotate([a,b,c,d,e,f,g,h],-2,X).
 * X = [g,h,a,b,c,d,e,f]
 * 
 * Hint: Use the predefined predicates length/2 and append/3, as well as the
 * result of problem 1.17.
 * 
 *)
fun rotate [] _ = []
  | rotate xs n =
      let
        val m = length xs
        val n' = n mod m
      in
        case (Int.sign n') of
          0 => xs
        | 1 => List.drop (xs, n') @ List.take (xs, n')
        | _ => List.drop (xs, m + n') @ List.take (xs, m + n')
      end
