(* old test case name: code15.sml *)

(* This is a typical example that would benefit from a merging of slices. *)
let
    val rec constList =
	fn x =>
	   fn n =>
              if (n = 0)
	      then [x, x]
              else x :: (tl (List.concat (constList (constList x (n-1)) (n-1))))
    val applyCl =
	fn l1 =>
	   fn l2 =>
	      fn f =>
		 ((constList l1 (f l1)), (constList l2 (f l2)))
in applyCl [1, 2, 3] [true, false, true] length
end
