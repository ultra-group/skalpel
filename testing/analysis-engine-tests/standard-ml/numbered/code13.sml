(* old test case name: code13.sml *)

let
    datatype ('a,'b) T = C of ('a * 'b)
    fun f (C (x,_)) (C (y,_)) z = (z x, z y)
in if true
   then f (C (1,2)) (C ((),4))
   else
       let
	   datatype U = f
           fun x _ = f
       in fn z => (z 1, z 2)
       end
end;
