(* untypable *)
structure S : sig val x : 'a end =
struct exception e val x = raise e end;
