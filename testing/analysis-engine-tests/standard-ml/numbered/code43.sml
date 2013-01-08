(* old test case name: code43.sml *)

(* f should be a value constructor because of "(f, f)"
   but is not because of the "rec f" *)
datatype t = f
val rec f = fn (f, f) => f;
