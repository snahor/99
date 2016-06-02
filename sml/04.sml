(* derp length *)
fun recursivelength xs =
  let
    fun aux ([], acc) = acc
      | aux (y :: nil, acc) = acc + 1
      | aux (y :: ys, acc) = aux(ys, acc + 1)
  in
    aux (xs, 0)
  end

(* a better length *)
val length = foldl (fn (_, acc) => acc + 1) 0;
