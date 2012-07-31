(* old test case name: code302.sml *)

(* All three expressions are untypable. *)
fn y =>
y
(
if y then 1 else 2
);
(* Same code, different layout. *)
fn y => y (if y then 1 else 2);
(* In the following piece of code we don't want any box around 'true'. *)
(fn x => x true) (fn x => x + 1);
