(* Only the first (b a) is untypable. *)
datatype t = a | b of t;
fun a () = ();
b a;
datatype t2 = datatype t;
b a;
