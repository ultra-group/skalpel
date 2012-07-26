(* closure example *)
let val f = fn x => let val y = x in y 5 end
in f 3
end
