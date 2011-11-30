(* typable *)
functor f (s : sig end) = struct end;
structure S = f(Int);
