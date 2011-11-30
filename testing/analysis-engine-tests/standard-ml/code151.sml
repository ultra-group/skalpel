(* These are ill-typed.
 * In the second line 'a can't be generalised because the exp
 * is expansive and the binding of 'a is at "val x" *)
let exception e of 'a in e end;
val x = (let exception e of 'a in e end; fn x => x : 'a -> 'a);
(* But these two are OK because the expressions are not expansive.
 * The expressions are in fact match rules because in a 'fun'
 * and match rules are never expansive. *)
fun ex x = (let exception e of 'a in e x end; fn x => x : 'a -> 'a; 1);
fun 'a ex x = (let exception e of 'a in e x end; 1);


(* SML/NJ just generates a dummy type for 'a and output a warning *)
(* HaMLet complains about undetermined types on top level *)
val ex = let val myfun : 'a -> 'a = fn x => x in myfun end;
(* This is beacuse the returned myfun is an instance of the
 * declared myfun but we don't know which generalisation. *)
