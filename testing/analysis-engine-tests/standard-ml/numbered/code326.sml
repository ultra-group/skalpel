(* Typable if IntListSet if the SML/NJ structure *)
signature ORDSET =
sig
    type ordset
    val isEmpty : ordset -> bool
end;

structure OrdSet :> ORDSET =
struct
structure S = IntListSet
type ordset = S.set
val isEmpty = S.isEmpty
end;

structure foo :> sig val foobar : unit -> unit end =
struct
structure O = OrdSet
fun foobar _ =
    let
	val strictLab = false
	fun isEmpty ll = strictLab andalso O.isEmpty ll
	fun foo (x : O.ordset) = isEmpty x
    in ()
    end
end
