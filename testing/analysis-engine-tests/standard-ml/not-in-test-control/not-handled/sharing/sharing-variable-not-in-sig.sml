(* error because x isn't declared in the signature *)



signature SIG1 =
sig
    eqtype t1
    eqtype t2
    sharing type t2 = x = t3
    eqtype t3
end;

structure STR1 : SIG1 =
struct
type t1 = int
type t2 = int
type t3 = bool
end;
