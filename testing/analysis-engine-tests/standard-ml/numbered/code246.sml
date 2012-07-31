(* old test case name: code246.sml *)

(* This is not typable because T1 does not appear in the signature *)
signature S = sig type t type u end;
structure st1 : S = struct datatype t = T1 of int | T2 type u = int end;
st1.T1;
