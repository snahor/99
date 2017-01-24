(*
 * (medium) Pack consecutive duplicates of list elements into sublists.
 *
 * If a list contains repeated elements they should be placed in separate
 * sublists.
 * 
 * Example:
 * ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
 * X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
 *)
local
  fun f (x, []) = [[x]]
    | f (x, (y :: ys) :: yss) =
        if x = y
        then (x :: y :: ys) :: yss
        else [x] :: (y :: ys ) :: yss
in
  fun pack xs = foldr f [] xs
end

fun pack' xs =
  let
    fun aux (x, []) = [[x]]
      | aux (x, ys :: yss) =
          if x <> (hd ys)
          then [x] :: ys :: yss
          else (x :: ys) :: yss
  in
    foldr aux [] xs
  end
