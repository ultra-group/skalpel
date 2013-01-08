(* old test case name: code332.sml *)

(* EXAMPLE1 *)
fn x => fn a => (x a, x 2, [x, 2]);
(* EXAMPLE2 *)
fun f (c::cs) (i::is) = if i > 0
			    then f cs is
			    else (c::[2.2]) @ f is cs;
(* EXAMPLE3 *)
fun m f (h::t) = f h::f t;
(* EXAMPLE4 *)
let val f = fn x => let val y = x
		    in y 5 end
in f 3 end;
(* EXAMPLE5 *)
fn x => let val y = (fn z => let val _ = x z
			     in (fn w => w) end)
	in (y 5, y true) end;
(* EXAMPLE6 *)
fn x => let val f = fn y => y x
	in (f (fn z => z)) (f (fn u => fn v => u)) end;
(* EXAMPLE7 *)
fn x => let val f = fn y => y x val g = fn z => z x
	in (f (fn w => w + 1), g (fn u => not u)) end;
(* EXAMPLE8 *)
fn z => let val x = z
	in (let val y = z 1 in x true end) end;
(* EXAMPLE9 *)
fun f y (h::t) = t y;
(* EXAMPLE10 *)
fun f5 0 n = []
  | f5 m n = m ^ n :: (f5 (m-1));
(* EXAMPLE11 *)
fun f5 x = if x>3 then x else 1.1;
(* EXAMPLE12 *)
fun f4 [] = []
  | f4 (0::t) = f4 t
  | f4 (h::t) = h/2.0::f4 t;
(* EXAMPLE13 *)
fun fail3 p nil = p + p
  | fail3 p (h::t) = if p true then [h] else t;
(* EXAMPLE14 *)
fun f 1 x = 0.0
  | f n x = (n*x) + (f (n-1) x);
(* EXAMPLE15 *)
fn x => if x then x+1 else x-2.2;
(* EXAMPLE16 *)
fn x => (x+1) (if x then x+1 else x-2.2);;
