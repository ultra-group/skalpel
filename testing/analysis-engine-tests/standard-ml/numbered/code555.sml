(* old test case name: code555.sml *)

(* Typable *)

structure S =
  struct type t = int type u = int fun f (x : t) = x val z : u = 1 end
  : sig type t type u val f : t -> t val z : u end;
S.f S.z;

(* Untypable *)

structure T =
  struct type t = int type u = bool fun f (x : t) = x val z : u = true end
  : sig type t type u val f : t -> t val z : u end;
T.f T.z;
