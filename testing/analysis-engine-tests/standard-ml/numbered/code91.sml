(* old test case name: code91.sml *)

fun fsemty [etv as {ty, chk = (T.VAL b), asmp, lab}] =
    compone (E.getVAL (plusproj (E.getG env1) id)) env2 cs2 [id] etv b (SOME (T.freshtyvar ()))
