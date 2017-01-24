(*
 * (medium) Decode a run-length encoded list.
 *
 * Given a run-length code list generated as specified in problem 1.11.
 * Construct its uncompressed version.
 *)
use "1.11.sml";

fun decode []        = []
  | decode (x :: xs) =
      case x of
        Single e        => e :: decode xs
      | Multiple (n, e) => List.tabulate (n, fn _ => e) @ decode xs
