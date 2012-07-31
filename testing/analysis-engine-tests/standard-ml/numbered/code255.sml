(* old test case name: code255.sml *)

structure Z =
  struct
    structure X =
      struct
	val x = (fn w => w) (fn u => u)
      end
    structure Y =
      struct
	val y = X.x 1
      end
  end;

structure X =
  struct
    val x = (fn w => w) (fn u => u)
  end
structure Y =
  struct
    val y = X.x 1
  end
