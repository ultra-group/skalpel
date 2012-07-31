(* old test case name: code260.sml *)

(* typable *)
let
    fun f () = let datatype t = A | B of t in (A, B) end;
    val (a, b) = f ()
in b a end;
let
    fun f () = let datatype t = A | B of t in (A, B) end;
    val (a, b) = f ();
    val (c, d) = f ()
in (b a, d c) end;
let
    fun f () = let datatype t = A | B of t in (A, B) end;
    val (a, b) = f ();
    val (c, d) = f ()
in (b a, d c, d a) end;
