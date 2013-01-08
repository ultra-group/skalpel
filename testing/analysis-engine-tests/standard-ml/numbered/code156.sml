(* old test case name: code156.sml *)

(* this seems to be a polymorphic exception *)
let val x = fn _ : 'a => let exception e of 'a in e end
in (raise x 1 1; raise x true true; 1) end;

(* this is valid but E is not supposed to be an exception when not applied *)
val x = fn _ : 'a => let exception E of 'a in E end
val it = (raise x 5 5) handle E => ()

(* we shouldn't accept that because it is unsound *)
exception E of 'a
val it = (raise E 5) handle E f => f (2);
