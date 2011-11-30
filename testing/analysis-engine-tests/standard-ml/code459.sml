(* Untypable *)
structure S = struct
  datatype 'a t = T of 'a t
  datatype 'a u = U
  val rec g = fn v => v
  val rec f = fn v => T g
end
