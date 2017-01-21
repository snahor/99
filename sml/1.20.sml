(*
 * (easy) Remove the K'th element from a list.
 *
 * Example:
 * ?- remove_at(X,[a,b,c,d],2,R).
 * X = b
 * R = [a,c,d]
 * 
 *)
fun removeNth xs n =
  let
    val pairs = ListPair.zip (xs, List.tabulate (length xs, fn x => x + 1))
  in
    map (#1) (List.filter (fn (x, i) => i <> n) pairs)
  end
