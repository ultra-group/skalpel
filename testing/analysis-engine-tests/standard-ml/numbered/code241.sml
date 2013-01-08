(* old test case name: code241.sml *)

signature S1 = sig type t type v end;
signature S2 = sig type u type v end;
structure st1 : S1 = struct type u = int type t = bool type v = bool end;
structure st2 : S2 = struct type u = int type t = bool type v = bool end;
1 : st1.t;
1 : st2.t;;
