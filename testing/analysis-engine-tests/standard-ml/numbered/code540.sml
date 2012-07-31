(* old test case name: code540.sml *)

(* Typable *)

signature e = sig
    type 'a t
    type 'a u = 'a list t
end

structure E :> e = struct
type 'a t = 'a list
type 'a u = 'a list t
end
