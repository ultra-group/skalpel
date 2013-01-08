(* old test case name: code71.sml *)

(* Untypable: because of, among other things, value polymorphism *)
let val (e', f) =
	let exception e of 'a
	in (e, fn g => g() handle e(x) => x)
	end
    val X = f(fn () => raise e'(7))
in
    X(2)+3
end;
