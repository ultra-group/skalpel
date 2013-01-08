(* old test case name: code452.sml *)

(* Untypable - Opening of an empty structure *)
structure T = struct end
structure S = struct
    datatype t = c of bool
    open T
    val c = fn x => x + 1
    val _ = c ()
end
open S;
