signature SIG1 =
sig
    datatype t1 = X | Y of int
    type t2
    sharing type t1 = t2
end;
