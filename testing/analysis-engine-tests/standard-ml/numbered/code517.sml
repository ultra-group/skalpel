(* old test case name: code517.sml *)

(* Typable *)
signature s1 = sig type t end
structure S1 :> s1 = _S1

signature s2 = sig type t val g : t -> unit end
structure S2 :> s2 where type t = S1.t = _S2

signature s3 = sig val f : S1.t end
structure S3 :> s3 = _S3

val _ = S2.g S3.f;
