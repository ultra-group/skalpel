(* old test case name: code268.sml *)

functor F (X : sig end) = struct end;
functor F (X : sig type int end) = struct open X; val _ = 1 : int end;
functor F (X : sig type int end where int = int) = struct open X; val _ = 1 : int end;
functor F (X : sig type int end where type int = int) = struct open X; val _ = 1 : int end;
