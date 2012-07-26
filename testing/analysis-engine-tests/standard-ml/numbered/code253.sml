signature SIG1 = sig type t val C : t -> t val D : t end
signature SIG2 = sig eqtype t val C : t -> t val D : t end
structure S = struct datatype t = C of t | D end :> SIG1 :> SIG2
val _ = S.C S.D
val _ = S.D S.C
