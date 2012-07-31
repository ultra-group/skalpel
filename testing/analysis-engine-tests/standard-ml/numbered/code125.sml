(* old test case name: code125.sml *)

(* this is allowed and well-typed with SML/NJ*)
(* the same can be obtained with the pair (not, map) for example *)
let
    datatype u = nil | true | z of u
in
 fn () =>
    let
	fun f x y c = (c x, c y)
    in f nil true z
    end
end;
