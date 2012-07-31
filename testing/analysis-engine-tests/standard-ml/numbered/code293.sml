(* old test case name: code293.sml *)

(* untypable: status clash *)
structure S = struct fun f () = () end
exception e
val (S.f x) = raise e
val S.f = raise e
(* untypable: status clash *)
structure S = struct exception e fun x () = 2 end
exception e = S.x
