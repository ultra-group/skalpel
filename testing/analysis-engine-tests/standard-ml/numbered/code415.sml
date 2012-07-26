(* Untypable *)
structure K = struct structure P = struct val x = 1.5 end end
structure A = struct open H I J K L open M N O P end
val x = true
open A B C D E F G
val y = x + 2
