(* typable *)
structure S = struct val x = 1 end
fun f p = (p S.x) : 'b
