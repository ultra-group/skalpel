(* Untypable, unmatched type b from specification. *)

signature S = sig
    type a
    type b
end

signature S2 = sig
    include S
    type c
end

structure test : S2 =
struct
    type a = int
    type c = int
end
