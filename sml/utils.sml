fun pow_int (a, b) =
  let
    fun aux (x, i) =
      if i = b
      then x
      else aux (x * a, i + 1)
  in
    aux (1, 0)
  end

val op ** = pow_int
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

fun takeWhile _ []        = []
  | takeWhile p (x :: xs) = if p x then x :: takeWhile p xs else []

fun dropWhile _ []        = []
  | dropWhile p (x :: xs) = if not (p x) then x :: xs else dropWhile p xs

(*
 * span is the same as:
 * span p xss = (takeWhile p xss, dropWhile p xss)
 *)
fun span _ []        = ([], [])
  | span p (x :: xs) =
      let
        val (ys, zs) = span p xs
      in
        if p x
        then (x :: ys, zs)
        else ([], x :: xs)
      end

(* span (fn x => x < 3) [1,2,3,4,1,2,3,4]; *)

fun group _   []        = []
  | group cmp (x :: xs) =
      let
        val (ys, zs) = span (fn x' => cmp (x, x')) xs
      in
        (x :: ys) :: group cmp zs
      end
