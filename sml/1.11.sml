(*
 * (easy) Modified run-length encoding.
 *
 * Modify the result of problem 1.10 in such a way that if an element has no
 * duplicates it is simply copied into the result list. Only elements with
 * duplicates are transferred as [N,E] terms.
 *
 * Example:
 * ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
 * X = [[4,a],b,[2,c],[2,a],d,[4,e]]
 *)

datatype 'a encoded_element = Single of 'a
                            | Multiple of int * 'a

local
  fun aux (1, x) = Single x
    | aux (n, x) = Multiple (n, x)
in
  fun encodeModified xs = map aux (encode xs)
end

fun encodeModified' xs =
  map (fn (n, x) => if n = 1 then Single (x) else Multiple (n, x)) (encode xs)
