(*
 * (***) Huffman code.
 * First of all, study a good book on discrete mathematics or algorithms for a
 * detailed description of Huffman codes, or consult Wikipedia
 * 
 * We suppose a set of symbols with their frequencies, given as a list of
 * fr(S,F) terms. Example:
 * [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is
 * to construct a list hc(S,C) terms, where C is the Huffman code word for
 * the symbol S. In our example, the result could be Hs = [hc(a,'0'),
 * hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')]
 * [hc(a,'01'),...etc.]. The task shall be performed by the predicate
 * huffman/2 defined as follows: 
 * 
 * % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
 *)

datatype tree = Leaf of char * int
              | Node of int * tree * tree

fun weight (Leaf (_, n))    = n
  | weight (Node (n, _, _)) = n

fun merge t0 t1 = Node ((weight t0) + (weight t1), t0, t1)

fun insert t []        = [t]
  | insert t (x :: xs) =
      case Int.compare (weight t, weight x) of
        GREATER => x :: (insert t xs)
      | _       => t :: x :: xs

fun sort []  = []
  | sort [x] = [x]
  | sort xs  = 
      let
        fun frequency x = #2 x

        fun merge ([], ys)           = ys
          | merge (xs, [])           = xs
          | merge (x :: xs, y :: ys) = 
              if frequency x < frequency y
              then x :: merge (xs, y :: ys)
              else y :: merge (x :: xs, ys)

        val half = length xs div 2
     in
        merge (sort (List.take (xs, half)), sort (List.drop (xs, half)))
     end

fun buildTree freqs =
  let
    (* convert freqs in a list of Leaf in asc order *)
    val leafs = map Leaf (sort freqs)

    (* recursively merge and insert in a new list*)
    fun aux (t :: nil)       = t
      | aux (t0 :: t1 :: ts) = (
          (print (
            ("t0: " ^ (PolyML.makestring t0) ^ "\n") ^
            ("t1: " ^ (PolyML.makestring t1) ^ "\n")));
          aux (insert (merge t0 t1) ts)
      )

  in
    aux leafs
  end

fun encode t =
  let
    fun aux t bits =
      case t of
        Node (_, left, right) => (aux left (bits ^ "0")) @ (aux right (bits ^ "1"))
      | Leaf (c, _)           => [(c, bits)]
  in
    aux t ""
  end

val huffman = encode o buildTree

(*
 * val fs = [(#"i", 5),
 *           (#"s", 4),
 *           (#"p", 2),
 *           (#"r", 2),
 *           (#" ", 1),
 *           (#"m", 1),
 *           (#"e", 1),
 *           (#"v", 1)];
 *
 * huffman fs;
 * 
 * val ws = [(#"E", 90),
 *           (#"L", 42),
 *           (#"D", 42),
 *           (#"U", 37),
 *           (#"X", 30),
 *           (#"M", 24),
 *           (#"K", 7),
 *           (#"Z", 2)];
 *
 * huffman ws;
 *
 * val output = [(#"L", "00"),
 *               (#"X", "010"),
 *               (#"Z", "01100"),
 *               (#"K", "01101"),
 *               (#"M", "0111"),
 *               (#"U", "100"),
 *               (#"D", "101"),
 *               (#"E", "11")]
 * 
 *)
