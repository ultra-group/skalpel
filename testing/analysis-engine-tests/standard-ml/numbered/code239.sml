(* old test case name: code239.sml *)

(* This is untypable because 'a cannot be generalized
   and e's type is not generalised either *)
let exception e of 'a in e 4 end;
(* This is still untypable because e's type is not generalised
   and so 'a is not int *)
fun f _ = let exception e of 'a in e 4 end;
(* However, the next one is typable *)
fun f _ = let exception e of 'a in e end; f () 1;;
