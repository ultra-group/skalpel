(* old test case name: code463.sml *)

(* Untypable - Type constructor clash involving -> *)
structure S = struct
  datatype 'a t = U of 'a
  datatype 'a t = T of 'a t
  val rec g = fn v => v
  val rec f = fn v => let val rec h = fn U x => T x
		      in h (U g)
		      end
end;
