(* error because x isn't declared in the signature *)

type x = int

signature SIG1 =
sig
    eqtype t1
    eqtype t2
    sharing type t2 = x
end;

structure STR1 : SIG1 =
struct
type t1 = int
type t2 = bool
end;
