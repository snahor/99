(*
 * (medium) Run-length encoding of a list (direct solution).
 *
 * Implement the so-called run-length encoding data compression method
 * directly. I.e. don't explicitly create the sublists containing the
 * duplicates, as in problem 1.09, but only count them. As in problem 1.11,
 * simplify the result list by replacing the singleton terms [1,X] by X.
 *
 * Example:
 * ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
 * X = [[4,a],b,[2,c],[2,a],d,[4,e]]
 *)
use "1.11.sml";

local
  fun aux (x, [])      = [Single x]
    | aux (x, y :: ys) =
        case y of
          Single e =>
            if e = x
            then (Multiple (2, e)) :: ys
            else (Single x) :: y :: ys
        | Multiple (n, e) =>
            if e = x
            then (Multiple (n + 1, e)) :: ys
            else (Single x) :: y :: ys
in
  fun encodeDirect xs = foldr aux [] xs
end
