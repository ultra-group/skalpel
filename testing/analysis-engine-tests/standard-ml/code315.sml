(* EXAMPLE1
 * We can have hidden specifications as follows: *)
(**SML-TES-SPEC val x : int*)
(**SML-TES-SPEC structure S : sig val y : bool end*)
fun f z = (z x, z S.y)

(* EXAMPLE2
 * We can load a file as follows: *)
(**SML-TES-SMLNJ-SCRIPT foo.sml*)
val _ = x + 1
