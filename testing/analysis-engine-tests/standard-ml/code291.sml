(* untypable because an int is not a bool *)
structure S :> sig type t = bool end = struct type t = int end
val _ = 1 : S.t
