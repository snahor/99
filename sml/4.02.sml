(*
 * (medium) Construct completely balanced binary trees
 *
 * In a completely balanced binary tree, the following property holds for every
 * node: The number of nodes in its left subtree and the number of nodes in its
 * right subtree are almost equal, which means their difference is not greater
 * than one.
 * 
 * Write a predicate cbalTree/2 to construct completely balanced binary trees
 * for a given number of nodes. The predicate should generate all solutions via
 * backtracking. Put the letter 'x' as information into all nodes of the tree.
 * Example:
 * ?- cbalTree(4,T).
 * T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
 * T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
 * etc......No
 *
 *)

datatype 'a bintree = Empty
                    | Node of 'a * 'a bintree * 'a bintree


fun cbalTree 0 = [Empty]
  | cbalTree 1 = [Node ("x", Empty, Empty)]
  | cbalTree n =
      let
        val tree = cbalTree ((n - 1) div 2)

        fun aux right left =
          foldr (fn (l, acc) => (
            foldr (fn (r, acc) => (Node ("x", l, r)) :: acc) acc right
          )) [] left
      in
        if (n - 1) mod 2 = 0
        then aux tree tree
        else
          let
            val tree' = cbalTree (n div 2)
          in
            aux tree tree' @ aux tree' tree
          end
      end
