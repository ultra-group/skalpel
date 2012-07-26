(* typable *)
datatype u = C | D of v withtype v = u;
D (C : v);
