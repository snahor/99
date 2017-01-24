(*
 * (medium) Extract a slice from a list.
 *
 * Given two indices, I and K, the slice is the list containing the elements
 * between the I'th and K'th element of the original list (both limits
 * included). Start counting the elements with 1.
 * 
 * Example:
 * ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
 *  L = [c,d,e,f,g]
 * 
 *)
fun slice xs i j =
  let
    fun aux _ []        = []
      | aux k (w :: ws) =
          case (i <= k, k <= j) of
            (true, true)  => w :: aux (k + 1) ws
          | (false, true) => aux (k + 1) ws
          | _             => []
  in
    aux 1 xs
  end
