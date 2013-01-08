(* old test case name: code350.sml *)

(* These two are untypable.
 * HaMLet says: "rigid type t". *)
signature S = sig type t = int end where type t = bool;
signature S = sig type t end where type t = bool where type t = int;;
