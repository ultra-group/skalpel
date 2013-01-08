(* old test case name: code66.sml *)

(* unresolved flexible record types *)
(* polymorphic records is a medium change in Successor ML *)
(* we don't catch this kind of error yet *)
let val f = fn {x = x, y = y, ...} => x + y in (f {x = 2, y = 3}, f {x = 3, y = 4}) end;
(* escaping local type name in let expression *)
(* sml/nj does some renamings and so does not care about these escapings *)
let datatype t = T in T end;
(* some explicit type variables cannot be generalised
   - it is a: val it = ...
   - because of the value polymorphism restriction
     (rule 15, a let expression is expansive) *)
let in fn x => x : 'a end;
(* undetermined types on top-level - this is a semantic error - rule 87 and 89 *)
(fn x => x) [];
(* two other type errors *)
(1; true; fn x => x; true);
it + 1;
(# 2 (1, true, 3)) + 1;;
