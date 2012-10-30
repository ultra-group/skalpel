functor F (X : sig type t = real; val x : t end)
  = struct
      val _ = (fn y => X.x = y)
    end
