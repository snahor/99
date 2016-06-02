fun reverse xs =
  let
    fun aux ([], acc) = acc
      | aux (y :: ys, acc) = aux (ys, y :: acc)
  in
    aux (xs, [])
  end
