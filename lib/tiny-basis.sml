(* DO NOT USE THIS BASIS LIBRARY - for debugging purposes only
 * this file should be deleted after debugging is complete *)

structure Basis :> sig

type unit = {}

eqtype int
type real

structure teststruct : sig
	      type 'a mytype
	      val y : int mytype
end

eqtype string
val ^         : string * string -> string

end = _structBasis

open Basis
