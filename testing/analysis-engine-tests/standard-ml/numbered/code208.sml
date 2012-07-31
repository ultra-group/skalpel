(* old test case name: code208.sml *)

(* 21.sml *)
(* derived from bug409.sml *)
(* type checking after functor application, nonstrict type constructor *)

signature BS =
sig
  type 'a t
  val f: int -> 'a t
end;

structure B =
struct
  type 'a t = bool * int
  fun f(x): 'a t = (true,x)
end;

functor F(X: BS) = struct end;

structure A = F(B);
