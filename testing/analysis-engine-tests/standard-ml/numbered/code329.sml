(* Typable *)
fun newTag () = let exception e of 'a in (e, fn (e x) => SOME x | _ => NONE) end;
val (aBuild, aCheck) = newTag ();
val (aBuild : int -> exn, aCheck) = newTag ();
val (bBuild : int -> exn, bCheck) = newTag ();
val c = [aBuild 1, bBuild 2];
bCheck (hd c);
bCheck (List.last c);
c;
val (cBuild : bool -> exn, cCheck) = newTag ();
val d = [aBuild 1, bBuild 2, cBuild true];
bCheck (List.last d);
aCheck (List.last d);
cCheck (List.last d);
