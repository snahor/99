(* 
 * (medium) Flatten a nested list structure.
 *
 * Transform a list, possibly holding lists as elements into a 'flat' list by
 * replacing each list with its elements (recursively).
 * 
 * Example:
 * ?- my_flatten([a, [b, [c, d], e]], X).
 * X = [a, b, c, d, e]
 * 
 * Hint: Use the predefined predicates is_list/1 and append/3
 * 
 *) 
datatype 'a nested = E of 'a
                   | L of 'a nested list

fun flatten xs =
  let
    fun aux (ws, acc) =
      case ws of
        E x   => x :: acc
      | L xs' => foldr aux acc xs'
  in
    aux (xs, [])
  end
