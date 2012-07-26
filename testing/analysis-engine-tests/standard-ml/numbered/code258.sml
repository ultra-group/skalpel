(* this is typable *)
signature s = sig type t end
structure S1 :> s = struct type t = int end
structure S2 : s = struct type t = S1.t end
structure S3 : s = struct type t = S1.t end
type a = S2.t
type b = S3.t
val _ = fn x : a => x : b
