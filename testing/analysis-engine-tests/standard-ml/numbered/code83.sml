(* old test case name: code83.sml *)

(* rule 43 of the Definition of SML: the identifier on the left of an "as"
   has to be a value variable *)
datatype t = x
fun f (x as (_, _)) = #1 x + 1;;
