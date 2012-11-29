(* the next two lines are typable *)
(* structure S : sig type t end = struct type t = int; val x : t = 6 end; *)
(* fun x (y:S.t) (z:S.t) = y = z; *)

(* this is NOT typable *)


structure S :> sig type t end = struct type t = int; val x : t = 6 end;
fun x (y:S.t) (z:S.t) = y = z;

(*
(* Skalpel wrongly finds non-minimal error slices.  There is a smaller slice that does not use the signature. *)
structure S : sig type t val x : t val y : t end = struct type t = unit val x = () val y = () end
fun f w z = z (w S.x) (w (fn q => S.y))
*)

(*
(* Skalpel wrongly failing to find error slice that does not use the signature. *)
structure S : sig end = struct val x = () end
val y = S.x ()
*)

(*
(* Skalpel wrongly misses the error which can be deduced just from the
 * fact that the type function t has 0 parameters and hence any uses of
 * it must be exactly the same type.  This is hard to do right and I
 * propose we not try to do it right. *)
structure S : sig type t val x : t val y : t end = T
fun f w z = z (w S.x) (w (fn q => S.y))
*)

(*
structure S : sig type 'a t val x : int t val y : bool t end = T
fun f w z = z (w S.x) (w S.y)
*)

(*
struct
  val x : int (Λα.α → α) = ()
end
*)

(*
fun f x w u = let val _ = x = x fun 'b z (y:'b) = u (w y) in () end;
*)

(*
(* REALLY REALLY BAD BUG!!!  'b can not be generalized where its
 * binding is placed because it must occur free in the type of w.
 * Skalpel fails to detect this error!  Auuugh!  Eeeeek!
 *)
fun f x w u = let val _ = x = x val 'b z = fn y : 'b => u (w y) in () end;
*)

(*
(* Skalpel gives 1 correct minimal slice not involving 'b, and 2 bizarre slices. *)
fun f x w = let val 'b z = fn y : 'b => w y in (z true, z 1) end;
*)

(* The slice Skalpel gives for this is not minimal and is bizarre in
 * multiple confusing ways.  For starters, how is w involved?  Other
 * things are also confusing. *)
(* fun f x w = let val 'b z = fn y : 'b => w y in z () end; *)

(*
(* Another Skalpel failure. *)
fun f x w u = let val _ = x = x fun 'b z (y:'b) = u (w x) (w y) in () end;
*)

(*
(* Skalpel fails to find this equality type error. *)
fun f x = x = x fun 'b z (y:'b) = f y
*)
