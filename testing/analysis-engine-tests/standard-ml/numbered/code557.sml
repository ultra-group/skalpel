(* old test case name: code557.sml *)

(* Untypable *)

functor F (type k val c : k) = struct val d = c end
structure S = F(type k = bool val c = true)
val lab = S.d : int


(* Similar *)

functor F (type k val c : k) = struct val d = c end
signature s = sig val d : int end
structure S : s = F(type k = bool val c = true)


(* Similar *)

functor F (val c : 'a) = struct val d = c end
signature s = sig val d : int end
structure S : s = F(val c = true)
