(* Untypable *)
functor F (S : sig type t val x : t -> int val y : t end) = struct
  val _ = S.x S.y
end;

structure T = F (struct type t = int val x = ! val y = 2.5 end);
