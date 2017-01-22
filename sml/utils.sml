fun expt_int (a, b) =
  let
    fun aux (x, i) =
      if i = b then x
      else aux (x * a, i + 1)
  in
    aux (1, 0)
  end

val op ** = expt_int
infix 6 **

(* port of Haskell's sequence *)
fun sequence 0 _  = [[]]
  | sequence 1 xs = map (fn x => [x]) xs
  | sequence n xs =
      foldr (fn (x, acc) => foldr (fn (ws, acc') => ([x] @ ws) :: acc')
                                  acc
                                  (sequence (n - 1) xs))
            [] xs

fun merge _ [] [] = []
  | merge _ xs [] = xs
  | merge _ [] ys = ys
  | merge cmp (x :: xs) (y :: ys) =
      if cmp (x, y)
      then x :: merge cmp xs (y :: ys)
      else y :: merge cmp (x :: xs) ys

fun sort _   []  = []
  | sort _   [x] = [x]
  | sort cmp xs  =
    let
      val m = length xs div 2
      val ys = sort cmp (List.take (xs, m))
      val ws = sort cmp (List.drop (xs, m))
    in
      merge cmp ys ws
    end
