(* old test case name: code172.sml *)

signature S = sig datatype t = C end;
structure st1 :> S = struct datatype t = C end;
structure st2 :  S = struct datatype t = C end;
val _ = st1.C = st2.C;
st1.C : st1.t;
st2.C : st2.t;
