datatype 'a nested = E of 'a | L of 'a nested list

fun flatten lst =
  let
    fun aux (xs, acc) =
      case xs of
        E(x) => x :: acc
      | L(xs') => foldr aux acc xs'
  in
    aux (lst, [])
  end
