(*
 * (easy) Check whether a given term represents a binary tree
 *
 * Write a predicate istree/1 which succeeds if and only if its argument is a
 * Prolog term representing a binary tree.
 * Example:
 * ?- istree(t(a,t(b,nil,nil),nil)).
 * Yes
 * ?- istree(t(a,t(b,nil,nil))).
 * No
 *)

datatype 'a bintree = Empty
                    | Node of 'a * 'a bintree * 'a bintree
