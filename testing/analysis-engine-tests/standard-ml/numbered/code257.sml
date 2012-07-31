(* old test case name: code257.sml *)

(* this is untypable and the two end points are t's first occurrence *)
signature s = sig type t end
structure S1 :> s = struct type t = int end
structure S2 :> s = struct type t = S1.t end
structure S3 :> s = struct type t = S1.t end
type a = S2.t
type b = S3.t
val _ = fn x : a => x : b
