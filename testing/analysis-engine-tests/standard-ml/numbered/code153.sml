(* old test case name: code153.sml *)

(* ISSUES WITH CLOSURE AND EXPLICIT TYPES *)

(* EXAMPLE1
 * Untypable because, among other things, of the value polymorphism
 * restriction.  The expression is expansive and cannot be generalised.
 * Moreover its type contains 'a because it returns e.
 * Because 'a is bound at the val declaration then the intersection
 * condition of rule 15 is not satisfied. *)
val (e', f) =
    let exception e of 'a
    in (e, fn g => g () handle e(x) => x) end
val X = f(fn () => raise e'(7));
X();

(* EXAMPLE2
 * Because of the last 'a, all the occurrences of this type variable
 * have to be bound to the outter val declaration.
 * This results in diverse type errors. *)
val ex2 = (let val id : 'a -> 'a = (fn _ => fn z => z) () in id id end;
	   fn z => z : 'a)

(* EXAMPLE3
 * Similar to the second example, but we don't obtain any circularity
 * error here. *)
val ex3 = (let val id : 'a -> 'a = fn z => z in id id end;
	   fn z => z : 'a)

(* EXAMPLE4
 * Untypable because 'a cannot be generalised where specified. *)
val 'a ex4 = (fn x => fn y : 'a => x) ()

(* EXAMLPLE5
 * Typable because 'a does no occur in the type of the expression. *)
val ex5 = ((fn x => fn y : 'a => x) (); 1);

(* EXAMPLE6
 * Typable because 'a can be generalised where specified. *)
fun 'a f () = let val _ = (fn x => fn y : 'a => x) () in () end;;
