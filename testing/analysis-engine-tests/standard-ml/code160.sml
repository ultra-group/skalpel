(* This is an error because the signature is more general than the structure *)
structure S :> sig val x : 'a end = struct val x = 1 end;
