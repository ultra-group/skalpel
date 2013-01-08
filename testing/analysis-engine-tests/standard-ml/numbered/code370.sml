(* old test case name: code370.sml *)

signature s = sig type 'a t end where type 'a t = int
(* typable *)
structure S :> s = struct type 'a t = int end
(* untypable *)
structure S :> s = struct type 'a t = bool end;
