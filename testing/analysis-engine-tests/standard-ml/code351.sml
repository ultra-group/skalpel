(* Untypable *)
signature s = sig type t end where type t = int
structure S : s = struct type t = bool end

(* Untypable *)
signature s = sig datatype t = C type u end where type t = int;
structure S : s = struct type t = int datatype u = C end;

(* Untypable *)
signature s = sig type u end where type u = int;
structure S : s = struct datatype u = C end;
