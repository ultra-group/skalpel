(* Typable *)

(**SML-TES-QUASIQUOTES true *);
Control.quotation := true;

structure SMLofNJ = struct
  datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a
end
val a = `xyzzy^(())plugh` : unit SMLofNJ.frag list
