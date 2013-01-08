(* old test case name: code498.sml *)

(* Typable *)
signature s = sig type t = int type u sharing type t = u end
structure S :> s = struct type t = int type u = int end

(* Untypable - arity error *)
signature s = sig type 'a t type u sharing type t = u end;

(* Untypable - type constructor clash *)
signature s = sig type t type u sharing type t = u end where type t = int
structure S :> s = struct type t = int type u = bool end

(* Untypable - type constructor clash *)
signature s = sig type t type u sharing type t = u end
structure S :> s = struct type t = int type u = t end
val _ = 1 : S.t;
