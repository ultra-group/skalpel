(* old test case name: code60.sml *)

(* value polymorphism restriction (section 4.8, rule 15):
   - the type of x cannot be generalized because the expression is expansive *)
let val x = (fn x => x) []
    val y = 1::x
    val z = true::x
in ()
end;
