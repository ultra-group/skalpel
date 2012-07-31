(* old test case name: code152.sml *)

(* EXAMPLE1
 * Intersection condition of rule 15 can fail because of something
 * else than exceptions.  For example in the following piece of code,
 * the expression is expansive and so cannot be generalised.
 * Moreover, its type contains the type variable 'a which, following
 * the binding rule for explicit type variables should be bound
 * at the value declaration.  This is not allowed in SML. *)
val x : 'a -> 'a = (fn _ => fn y => y) ();
(* Should we not obtain a free variable at top-level? *)

(* EXAMPLE2
 * We obtain the same problem with the following piece of code
 * because z has a monomorphic type and so the inner function
 * cannot be generalised. *)
fn z => let val x : 'a -> 'a = fn y => (z y) in () end;

(* EXAMPLE3
 * Using t makes one of the errors of EXAMPLE1 go away because
 * we then don't know if 'a t contains the type variable 'a.
 * As a matter of fact we can have the following declaration:
 * type 'a t = int *)
val x : 'a t -> 'a = (fn _ => fn y => y) ();

(* EXAMPLE4
 * This is typable because the type variable is explicitly
 * bound at the outter val declaration. *)
val 'a w = fn z => let val x : 'a -> 'a = (fn _ => fn y => y) () in () end;

(* EXAMPLE5
 * Untypable because 'a cannot be bound where specified
 * (the type of the expression contains 'a because not generalised). *)
val 'a ex4 = (fn x => fn y : 'a => x) ()

(* EXAMPLE6
 * Typable because 'a can be generalised where specified. *)
fun 'a f () = let val ex4 = (fn x => fn y : 'a => x) () in () end;

(* EXAMPLE7
 * Untypable because 'a is bound to the outter val declaration
 * and so at 'f 1' the type of f is not generalised and still
 * 'a -> 'a and 'a is different from int. *)
val 'a u = let val f = fn x : 'a => x in f 1 end;
