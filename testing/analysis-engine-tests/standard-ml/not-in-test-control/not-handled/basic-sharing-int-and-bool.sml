signature SIG1 =
sig
    eqtype t1
    eqtype t2
    sharing type t1 = t2
end;

structure STR1 : SIG1 =
struct
type t1 = int
type t2 = bool
end;
