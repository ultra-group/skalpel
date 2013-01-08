(* old test case name: code248.sml *)

(* This is typable *)
signature S = sig type t type u val T1 : int -> t end;
structure st1 : S = struct datatype t = T1 of int | T2 type u = int end;
st1.T1;;
