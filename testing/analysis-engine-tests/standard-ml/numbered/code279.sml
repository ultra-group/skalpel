(* This is typable *)
structure S = struct val x = true end
structure T = struct val x = 1 end
open S T
val _ = x + 1;
(* This is not *)
structure S = struct val x = true end
structure T = struct val x = 1 end
open T S
val _ = x + 1;
