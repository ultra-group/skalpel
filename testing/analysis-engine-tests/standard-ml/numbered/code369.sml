(* old test case name: code369.sml *)

(* Typable *)
signature s = sig type t end where type t = int -> int

(* Untypable *)
signature s = sig type 'a t val x : int t end
structure S : s = struct type 'a t = 'a val x = true end

(* typable *)
signature s = sig type 'a t val x : int t end
structure S : s = struct type 'a t = 'a val x = 1 end;
