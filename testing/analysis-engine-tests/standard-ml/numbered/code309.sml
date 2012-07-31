(* old test case name: code309.sml *)

(* Untypable *)
fn y =>
   let
       datatype t = T1 | T2 | T3 of int
       val T3 x = y 1
       val _ = fn z => (z y, z T2)
   in ()
   end;
