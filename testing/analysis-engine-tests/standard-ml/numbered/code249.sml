(* old test case name: code249.sml *)

(* untypable *)
signature S = sig datatype t = T1 of int | T2 of bool type u end;
structure st1 : S = struct datatype t = T1 of int | T2 of bool type u = int end;
structure st2 : S = st1;
st2.T2 1;
