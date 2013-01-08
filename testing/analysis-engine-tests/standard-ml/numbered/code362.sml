(* old test case name: code362.sml *)

(* Typable *)
datatype 'a option = NONE | SOME of 'a
structure S : _s = struct
fun foo (SOME x) = SOME (x ())
  | foo NONE = NONE
end;
