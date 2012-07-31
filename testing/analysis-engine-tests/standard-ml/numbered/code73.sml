(* old test case name: code73.sml *)

(* Untypable *)
let exception e of 'a
in (raise e(5)) handle e(f) => f(7)
end;
let val (e', f) =
	let exception e of 'a
	in (e, fn g => g() handle e(x) => x)
	end
    val X = f(fn () => raise e'(7))
in
    X(2)+3
end;
