(* old test case name: code271.sml *)

signature S0 = sig end
signature S1 = sig structure s : S0 end
signature S2 = sig structure s : S1 end
structure t = struct end
structure s = struct structure s = t end : S2 : S1
