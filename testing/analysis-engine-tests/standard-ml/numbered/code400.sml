(* old test case name: code400.sml *)

(* Typable *)
signature s = sig type t val x : t end
structure S : s = struct type t = bool val x = true end
open S
val _ = x : bool;

(* Untypable *)
signature s = sig type t val x : t end
structure S :> s = struct type t = bool val x = true end
open S
val _ = x : bool;;
