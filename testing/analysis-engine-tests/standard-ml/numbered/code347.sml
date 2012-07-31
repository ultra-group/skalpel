(* old test case name: code347.sml *)

(* EXAMPLE1: Typable *)
structure S : sig type 'a t end =
struct type 'a t = int end
(* EXAMPLE2: Untypable *)
structure S : sig type 'a t end =
struct type t = int list end
(* EXAMPLE3: Untypable *)
structure S : sig type 'a t end =
struct type t = int end
