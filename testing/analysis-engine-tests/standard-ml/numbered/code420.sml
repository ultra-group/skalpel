(* old test case name: code420.sml *)

signature s = sig type foo end
structure S :> s = struct type foo = int end
open S
type t = int foo;
