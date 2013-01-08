(* old test case name: code361.sml *)

(* typable *)
let
    datatype 'a option = NONE | SOME of 'a
    fun foo (SOME x) = SOME (x ())
      | foo NONE = NONE;
in ()
end;
(* untypable *)
let
    datatype 'a option = NONE | SOME of 'a
    fun NONE () = ();
    fun foo (SOME x) = SOME (x ())
      | foo NONE = NONE;
in ()
end;
(* untypable *)
let
    datatype 'a option = SOME of 'a
    fun NONE () = ();
    fun foo (SOME x) = SOME (x ())
      | foo NONE = NONE;
in ()
end;;
