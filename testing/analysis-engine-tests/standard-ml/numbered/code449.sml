(* old test case name: code449.sml *)

(* Untypable *)
let val rec f = fn x => x
in fn f => fn (f x) => x
end;
