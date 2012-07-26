(* value polymorphism error in the 2 following lines: *)
fun g1 () = let exception e of 'a in ((fn (e y) => y), e) end;
let val (x, y) = g1 () in (x (y true), x (y 1)) end;
(* value polymorphism error in the 2 following lines: *)
fun g2 () x = x;
let val x = g2 () in (x 1, x true) end;
(* no error in the following 2 lines: *)
fun g3 x = x;
let val x = g3 in (x 1, x true) end;
(* no error in the following 2 lines: *)
datatype 'a t = Nil of unit | Cons of 'a * 'a t;
let val x = Nil () in (Cons (1, x), Cons (true, x)) end;
