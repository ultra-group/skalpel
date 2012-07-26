(* this type check *)
signature S = sig type t end;
structure st1 :> S = struct type t = int end;
structure st2 :> S = struct type t = bool end;
structure st1' : S = struct type t = int end;
structure st2' : S = struct type t = bool end;
(* this does not type check *)
1 : st1.t;
(* this type check *)
1 : st1'.t;
(* this is because the environemnt associated to st1 does
   not rename the type names *)
