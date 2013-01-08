(* old test case name: code458.sml *)

(* Untypable *)
structure S = struct
  datatype 'a t = T
  datatype 'a w = W
  datatype 'a u = U of 'a t
  val rec f = fn v => U W
end;
