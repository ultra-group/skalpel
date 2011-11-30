(* x is a value constructor with argument but is used
   in the structure without any *)
datatype t = x of int | C of int;
structure S :> sig end =
struct val x = fn u => C u end;
