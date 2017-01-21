(*
 * (easy) Create a list containing all integers within a given range.
 *
 * Example:
 * ?- range(4,9,L).
 * L = [4,5,6,7,8,9]
 * 
 *)
fun range a b =
  if a <= b
  then List.tabulate (b - a + 1, fn x => x + a)
  else []
