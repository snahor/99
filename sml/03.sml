exception ElementNotFound

fun kth_element ([], _) = raise ElementNotFound
  | kth_element (x :: xs, k) = if k = 1
                               then x
                               else kth_element (xs, k - 1)
