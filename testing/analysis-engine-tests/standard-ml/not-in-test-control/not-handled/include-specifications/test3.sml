(** Untypable, unmatched specification b. *)

signature S = sig
    type a
end

signature S2 = sig
    include S
    include sig type b end
    type c
end

structure test : sig include S2 end =
struct
    type a = int
    type c = int
end
