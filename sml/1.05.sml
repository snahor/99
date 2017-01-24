(* 
 * (easy) Reverse a list.
 * 
 *) 
fun reverse xs =
  let
    fun aux ([], acc) = acc
      | aux (y :: ys, acc) = aux (ys, y :: acc)
  in
    aux (xs, [])
  end

fun reverse' xs = foldl (fn (x, acc) => x :: acc) [] xs
