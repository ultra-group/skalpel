(* error because x isn't declared in the signature *)

type x = int

signature SIG1 =
sig
    datatype x = A | B of int
    eqtype t1
    eqtype t2
    sharing type t2 = x
end;

structure STR1 : SIG1 =
struct
datatype x = A | B of int
type t1 = int
type t2 = bool
end;
