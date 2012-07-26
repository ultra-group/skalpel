(* This code generates the same problem as in: fun f x = x and g () = (f 1, f true).
   The functions f and g are not polymorphic while used in their declaration
   (the same declaration because of the use of "and"). *)
let val createList = fn x => [x]
    val removeList = fn x => hd x
    val rec comp  = fn f => fn g => f o g
    and appComp = fn v1 => fn v2 =>
        (comp removeList createList v1) = hd (comp createList removeList v2)
in appComp 5 [5] end
