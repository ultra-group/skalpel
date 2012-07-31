(* old test case name: code16.sml *)

let
    val rec f =
	fn (x,_) =>
	   fn (y,_) =>
	      fn z => (z x, z y)
in f (1,2) ((),4)
end
