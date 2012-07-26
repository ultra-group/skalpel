(* Untypable *)
signature S = sig datatype t = T1 of int | T2 of bool type u end;
structure st1 : S = struct datatype t = T1 of int | T2 of int type u = int end;
st1.T1 true; st1.T2 true; st1.T2 1;
