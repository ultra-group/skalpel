(* old test case name: code552.sml *)

(* Typable *)

datatype 'a my_list = NIL | !! of 'a * 'a my_list;
infixr !!;
fun myLength NIL = 0
  | myLength (_!!t) = 1 + myLength t;
if myLength (1!!2!!3!!4!!NIL) = 4 then print "yay\n" else print "oh noes!\n";

(**SML-TES-QUASIQUOTES false *);
Control.quotation := false;
val ` = 1;
val `` = ` + ` + `;
(**SML-TES-QUASIQUOTES true *);
Control.quotation := true;
val x = `abc^(``)def`;
(**SML-TES-QUASIQUOTES false *);
Control.quotation := false;
if length x = `` then print "yay\n" else print "on noes!\n";;
