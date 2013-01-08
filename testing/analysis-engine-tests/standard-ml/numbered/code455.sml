(* old test case name: code455.sml *)

(* Untypable *)
structure S = struct
  datatype 'a t = T
  datatype 'a u = U
  datatype 'a m = M
  structure T = struct val rec w = fn f => f T end
  val rec f =
   fn x =>
   fn y =>
   fn z => let val rec g1 = fn v => z T
	   in let val rec g2 = fn v => x z
	      in y z
	      end
	   end
  open T
  val rec g = fn y => f w y
  val rec h = fn f => f U
  val rec dummy = fn v => g h
end;
