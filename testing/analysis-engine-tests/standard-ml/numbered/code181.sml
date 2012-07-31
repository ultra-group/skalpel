(* old test case name: code181.sml *)

(* nice error report using smlnj *)
datatype int = zero | succ of int | pred of int;
fun fromIntToint 0 = zero
  | fromIntToint n = if n < 0
		     then pred (fromIntToint (n + 1))
		     else succ (fromIntToint (n - 1));
fromIntToint ~4;
(fromIntToint ~4) + 1;
