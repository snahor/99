(* 
 * (easy) Find out whether a list is a palindrome.
 *
 * A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
 * 
 *) 
use "1.05.sml";

fun isPalindrome xs = xs = reverse xs
