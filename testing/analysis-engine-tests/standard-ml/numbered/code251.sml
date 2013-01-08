(* old test case name: code251.sml *)

(* This is typable *)
signature S1 = sig type t end;
structure s1 = struct datatype t = T end;
signature S2 = sig structure st1 : S1 end;
structure s2 : S2 = struct structure st1 = s1 end;;
