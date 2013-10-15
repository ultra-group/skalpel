signature S1 = sig
    type a = int
end

signature S2 = sig
    type a = bool
    include S1
end

(* signature Sx = sig type z end *)

(* signature S1 = sig *)
(*     type a = int *)
(*     type c = string *)
(*     include Sx *)
(* end *)

(* signature S2 = sig *)
(*     type z = bool *)
(*     include S1 *)
(* end *)
