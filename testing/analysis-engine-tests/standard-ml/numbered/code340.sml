(* Typable but seems that no structure can have this signature. *)
signature s = sig datatype t = C type u end where type t = int;
(* Untypable because C is not declared in the structure. *)
structure S : s = struct type t = int type u = int end;
(* Untypable because t should be of type int. *)
structure S : s = struct datatype t = C type u = int end;
(* Untypable because C is a datatype constructor and not a
 * value variable. *)
structure S : s = struct type t = int type u = int val C = 1 end;
(* Typable but it seems that f cannot be applied. *)
functor f (x : s) = struct open x end : s;
(* Typable.  We now have a new contructor for int. *)
functor f (x : s) = struct open x fun g C = 1 val h = g 2 end : s;
