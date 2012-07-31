(* old test case name: code288.sml *)

structure S :> sig type t = bool end = struct type t = int end
val _ = 1 : S.t
