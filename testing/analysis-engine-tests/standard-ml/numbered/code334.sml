(* old test case name: code334.sml *)

(* untypable *)
fn x => fn c => fn u => fn v =>
let
    val _ = c u
    overload + : 'a * 'a  -> 'a with 'a in (int, real)
    val _ = u + v
in c true
end;
