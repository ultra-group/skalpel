(* old test case name: code341.sml *)

(* Typable. *)
signature s = sig
    structure S : INTEGER
    datatype t = C
    type u
end where type S.int = string;;
