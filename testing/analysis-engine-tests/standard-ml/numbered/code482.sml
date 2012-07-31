(* old test case name: code482.sml *)

(* Untypable - f is the same in S and T and is monomorphic *)
structure X = struct
  structure S = struct
    val f = (fn x => x) []
  end
  structure T = S
  val _ = 1 :: S.f
  val _ = true :: T.f
end
