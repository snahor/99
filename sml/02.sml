fun last_but_one (x :: y :: nil) = x
  | last_but_one (x :: xs) = last_but_one xs
  | last_but_one [] = raise List.Empty
