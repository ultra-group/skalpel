(* old test case name: code345.sml *)

(* true and false cannot be bound so HaMLet reject that.
 * SML/NJ does not type check it because S.true is of
 * type S.bool which is not bool. *)
structure S :> sig
    datatype bool = true | false
end = struct
datatype bool = datatype bool
end;
S.true : bool;

(* HaMLet reject that but SML/NJ is happy. *)
structure S :> sig
    datatype bool = true | false
end where type bool = bool
= struct
datatype bool = datatype bool
end;
S.true : bool;
