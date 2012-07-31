(* old test case name: code178.sml *)

signature SIG = sig type t val x : t -> t end;
structure STRUC1 = struct type t = int fun x 0 = 0 end;
structure STRUC2 = struct type t = bool fun x true = true end;
structure STRUC1 : SIG = STRUC1;
structure STRUC2 : SIG = STRUC2;
