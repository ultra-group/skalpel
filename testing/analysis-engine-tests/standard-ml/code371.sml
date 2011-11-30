type v = bool
datatype t = C of v withtype v = int
(* typable *)
val x1 = C 1
(* untypable *)
val x2 = C true
