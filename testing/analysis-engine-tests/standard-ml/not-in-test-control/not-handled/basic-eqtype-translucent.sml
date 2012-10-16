signature S =
sig
    eqtype t
end

structure T : S =
struct
    type t = real
end
