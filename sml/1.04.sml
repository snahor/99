(* 
 * (easy) Find the number of elements of a list.
 * 
 *) 
fun len xs =
  let
    fun aux ([], acc)       = acc
      | aux (y :: nil, acc) = acc + 1
      | aux (y :: ys, acc)  = aux (ys, acc + 1)
  in
    aux (xs, 0)
  end

val len' = foldl (fn (_, acc) => acc + 1) 0
