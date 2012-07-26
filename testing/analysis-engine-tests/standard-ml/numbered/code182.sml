(* closure example *)
fun f x = let val y = x in y 5 end
val _ = f 3;

(fn x => (x : int)) true;
fn x => (x: int, x : bool);

(* same problem *)
fun f x = (x, let val g = f in g 1 end)
val _ = f true;
