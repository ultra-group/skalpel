(* Typable *)
functor F (val t : int * int -> bool) = struct val _ = t (1,1) end;
structure S = F(val t = op = : int * int -> bool);
