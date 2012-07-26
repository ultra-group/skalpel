(* this is an error *)
fn () =>
   let
       fun f x y c = (c x, c y)
   in f nil true z
   end
