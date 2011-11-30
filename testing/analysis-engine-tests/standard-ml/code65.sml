(* untypable *)
val x : {1 : int, 2 : bool} = {1 = 2, 2 = 2}
val (x : bool, y, z) = {C = 3, 1 = 1, 2 = 2}
val (x : bool, y) = (1, 2)
val (x, y) = (1, 2, true)
val {x = x, y = y, ...} = {x = 1, z = 1, y = 1, v = 1}
val ex = (fn {x = x, y = y, ...} => x + y) {x = true, z = 1, v = 1, z = 1}
val (x : bool, y, z) = {3 = 3, 1 = 1, 2 = 2}
val x = #hex {he = true, asc = 2}
