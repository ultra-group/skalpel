(* old test case name: code270.sml *)

signature S1 = sig structure s : INTEGER end
signature S2 = sig structure s : (*INTEGER*) S1 end
structure s = struct structure s = Int end : S2 : S1
