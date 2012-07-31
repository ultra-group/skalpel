(* old test case name: code42.sml *)

let
    val b = case u of true => 1 | false => 0
    val x = true
in while b do x + 1
end
