(* this is not an equality type error
 * this is generating an error because we do not check the operator in ExpOp
 * add this check in and this test should pass *)
val x = 1.0;
x > 2.0;
