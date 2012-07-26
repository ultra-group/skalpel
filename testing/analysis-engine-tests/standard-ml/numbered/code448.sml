(* Untypable *)
structure S = struct
  structure S = struct
    structure S = struct datatype t = C of bool end
    open S
    val g = fn (C x) => if x then 1 else 0
    val v = g (C true)
  end
  structure S = struct
    datatype t = C of int
    open S
    val rec f = fn (C x) => x + v
  end
end
