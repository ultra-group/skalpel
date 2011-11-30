(* Untypable - Applied value variabel *)
structure S = struct
  datatype 'a t = c of 'a
  val rec f = fn c => c
end
