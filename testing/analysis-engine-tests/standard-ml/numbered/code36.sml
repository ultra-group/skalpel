(* old test case name: code36.sml *)

let
    datatype 'a u = C of 'a * 'a
    datatype t = D
    datatype t = E
in C (D, E)
end
