(* Contains a status clash:
 * exception can't be bound to a datatype constructor *)
datatype t = f
val f = 2
exception e = f
