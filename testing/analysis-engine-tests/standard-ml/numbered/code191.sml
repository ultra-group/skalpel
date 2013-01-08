(* old test case name: code191.sml *)

(* datatype constructors can be mentioned with val in signatures *)
structure X = struct datatype t = C end : sig type t val C : t end;

(* more typical way of mentioning datatype constructors in signatures *)
structure X = struct datatype t = C end : sig datatype t = C end;

(* this is illegal *)
structure X = struct datatype t = C end : sig datatype t = C val C : t end;

(* an example showing that a multi-occurrence error slice in a
   signature needs to include enough syntax to show what namespace
   the identifiers are living in. *)
structure X = struct datatype C = d; val C = d end : sig type C val C : C end;;
