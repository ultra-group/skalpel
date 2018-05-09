(* example type errors not containing even 1 expression *)

structure x : sig type t = int end = struct end;

structure x : sig type t = int end = struct type t = bool end;

type ('a,'b) t = ('a,'b) list;

(* example type errors where no minimal error slice contains an expression *)

fun f (x : int : bool) = ();

fun f (1 : bool) = ();

val _ = () handle true => ();

val _ = fn "hello" => () | () => ();

val _ = fn ("hello" | ()) => (); (* SML/NJ or-pattern *)

datatype t1 = A;
datatype t2 = B;
fun f A = () | f B = ();
