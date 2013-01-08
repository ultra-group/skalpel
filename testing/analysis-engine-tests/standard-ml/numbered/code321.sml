(* old test case name: code321.sml *)

(* This does not seem to be allowed in SML *)
structure S =
struct
(* This should be a strdec *)
functor F (X : sig end) = struct end;
end;
(* F is accessible outside the structure only with open. *);
