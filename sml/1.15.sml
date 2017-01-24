(*
 * (medium) Duplicate the elements of a list a given number of times.
 *
 * Example:
 * ?- dupli([a,b,c],3,X).
 * X = [a,a,a,b,b,b,c,c,c]
 * 
 * What are the results of the goal:
 * ?- dupli(X,3,Y).
 * 
 *)
fun replicate xs n = foldl (fn (x, acc) => acc @ List.tabulate(n, fn _ => x)) [] xs
