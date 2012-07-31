(* old test case name: code324.sml *)

(* Typable: *)
signature s = sig
    exception e of string
end
structure S : s = struct exception e of string end
val _ = () handle S.e _ => ()

(* Untypable: *)
structure S = struct datatype t = e of string end
exception e = S.e
