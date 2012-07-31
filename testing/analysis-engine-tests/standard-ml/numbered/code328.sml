(* old test case name: code328.sml *)

datatype t = T | U of int;
(* Syntacticly correct: *)
fun f T x = x | f _ _ = 0;
(* Syntacticly incorrect (not same number of arguments
 * and U used without argument): *)
fun f U x = x | f _ = 0;
fun f U x = x | f T = 0;
(* Syntacticly correct: *)
fun f (U x) = x | f T = 0;
(* Syntacticly incorrect (U used without arguments): *)
fun f U x = x | f _ _ = 0;
(* Syntacticly correct: *)
fun f T = 1 | f (U x) = x;
