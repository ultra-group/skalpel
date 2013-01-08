(* old test case name: code454.sml *)

(* Untypable - Monomorphic functions *)
structure S = struct
  datatype 'a t = T
  datatype 'a u = U
  val rec dummy1 =
   fn z =>
   fn v1 =>
   fn v2 =>
      let val rec dummy2 =
	   fn v => let val rec y = fn f => f z
		   in let val rec dummy4 =
			   fn v => let val rec z = fn f => f T
				   in z v1
				   end
		      in y v1 (* forces the first z to be an T *)
		      end
		   end
      in let val rec dummy6 = fn v => v2 z
	 in v2 U (* forces the first z to be a U *)
	 end
      end
end;
