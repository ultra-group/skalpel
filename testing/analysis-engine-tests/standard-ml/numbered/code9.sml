(* typable *)
let
    datatype ('a, 'b, 'c, 'd) T = C of int * 'c * 'c * 'd | D
    and u = E
in fn D => 1
end
