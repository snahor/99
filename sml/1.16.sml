(*
 * (medium) Drop every N'th element from a list.
 *
 * Example:
 * ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
 * X = [a,b,d,e,g,h,k]
 * 
 *)

fun dropEveryNth xs n =
  let
    val pairs = ListPair.zip (xs, List.tabulate (length xs, fn x => x + 1))
  in
    map (#1) (List.filter (fn (x, i) => i mod n <> 0) pairs)
  end

fun dropEveryNth' xs 0 = xs
  | dropEveryNth' xs n =
    let
      fun aux _ []        = []
        | aux i (w :: ws) =
            if i mod n = 0
            then aux (i + 1) ws
            else w :: aux (i + 1) ws
    in
      aux 1 xs
    end

