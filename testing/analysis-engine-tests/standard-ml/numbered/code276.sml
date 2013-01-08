(* old test case name: code276.sml *)

(* untypable *)
fun update (key:int,value,rest) (k,v) =
    if k=key
    then (value,update (key,value,rest))
    else
	let val (v,r) = rest (k,v)
	in (v,update (key,value,r))
	end;

fun extend k v = (v,update (k,v,extend));

fun build 0 u = u
  | build n u = build (n-1) (update (n,n+1,u));

val upd = build 10 extend;

fun test 0 u = []
  | test n u = let val (v,_) = u (n,n) in v::test (n-1) u end;

val _ = test 10 upd;;
