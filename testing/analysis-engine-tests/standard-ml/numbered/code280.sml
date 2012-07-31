(* old test case name: code280.sml *)

(* Status clash *)
(* This is not typable, SML/NJ complains that S.C is not a constructor
 * but a variable.
 * It's not typable with a translucent signature either. *)
structure S :> sig
    type t val C : int -> t
end = struct datatype t = C of int end;
val (S.C x) = S.C 1;
(* However this is typable *)
structure S :> sig
    datatype t = C of int
end = struct datatype t = C of int end;
val (S.C x) = S.C 1;
