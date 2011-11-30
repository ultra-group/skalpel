(* Typable *)
val e = fn () => let exception e of 'a in e end;
val _ = raise e () 1;
val _ = raise e () true;
