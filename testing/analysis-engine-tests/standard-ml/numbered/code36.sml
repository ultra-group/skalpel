let
    datatype 'a u = C of 'a * 'a
    datatype t = D
    datatype t = E
in C (D, E)
end
