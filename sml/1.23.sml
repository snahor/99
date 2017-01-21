(*
 * (medium) Extract a given number of randomly selected elements from a list.
 *
 * The selected items shall be put into a result list.
 * Example:
 * ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
 * L = [e,d,a]
 * 
 * Hint: Use the built-in random number generator random/2 and the result of
 * problem 1.20.
 * 
 *)

use "1.20.sml";

(* park-miller aka lehmer random number generator *)
local
  val a = 16807.0
  val m = 2147483647.0
  val q = 127773.0
  val r = 2836.0

  fun f seed =
    let
      val hi = Real.realTrunc (seed / q)
      val lo = seed - q * hi
      val test = a * lo - r * hi
    in
      if test > 0.0
      then test
      else (test + m)
    end
in
  fun randomGenerator seed =
    let
      val state = ref seed
    in
      fn (_: unit) => (state := f (!state); !state / m)
    end
end

fun randomSelect xs n =
  let
    val seed = Time.toReal (Time.now ())

    val nextRandom = randomGenerator seed

    (* calling the generator because the first run value is too small, and
     * randint will produce 0 *)
    val _ = nextRandom ()

    fun randint min max = Real.trunc (nextRandom () * (max  - min) + min)

    fun loop 0 _  = []
      | loop m ws =
          let
            val i = randint 0.0 ((Real.fromInt o length) ws)
          in
            List.nth (ws, i) :: loop (m - 1) (removeNth ws (i + 1))
          end
  in
    loop n xs
  end

