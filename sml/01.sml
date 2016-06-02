fun last (x :: nil) = x
  | last (x :: xs) = last xs
  | last [] = raise List.Empty
