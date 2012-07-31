(* old test case name: code352.sml *)

(* This is correct according to the definition of SML and
 * HaMLet is happy.  However, SML/NJ does not seem to parse it. *)
signature S = sig type t end where type t = int and T = sig end
