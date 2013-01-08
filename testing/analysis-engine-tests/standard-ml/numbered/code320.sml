(* old test case name: code320.sml *)

(* Untypable *)
fn z => let val x = fn y : 'a => (z y) in () end;
(* Typable *)
fn z => let val _ = fn y : 'a => (z y) in () end;
(* Untypable *)
fn z => let val f = fn x => z x in (f 1, f true) end;;
