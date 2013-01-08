(* old test case name: code531.sml *)

(* Untypable because of the value polymorphism restriction. *)
(* Similar to 530 but a bit more longer. *)

(* === FLAGS === *)
structure Flag = struct
  datatype col = Blue   of int
	       | White  of int
	       | Red    of int
	       | Black  of int
	       | Yellow of int

  type flag = {col1 : col, col2 : col, col3 : col}

  fun func x = x
  fun plus x = x + 1

  fun map (Blue   n) f = Blue   (f n)
    | map (White  n) f = White  (f n)
    | map (Red    n) f = Red    (f n)
    | map (Black  n) f = Black  (f n)
    | map (Yellow n) f = Yellow (f n)

  fun enlarge {col1, col2, col3} =
      {col1 = map col1 func,
       col2 = map col2 plus,
       col3 = map col3 func}

  val flag = enlarge {col1 = Blue 1, col2 = White 1, col3 = Red 1}

  fun toString (Blue   n) = "Blue("   ^ Int.toString n ^ ")"
    | toString (White  n) = "White("  ^ Int.toString n ^ ")"
    | toString (Red    n) = "Red("    ^ Int.toString n ^ ")"
    | toString (Black  n) = "Black("  ^ Int.toString n ^ ")"
    | toString (Yellow n) = "Yellow(" ^ Int.toString n ^ ")"
end


(* === COUNTRIES === *)
structure Country = struct
  datatype country = France
		   | Germany
		   | Spain
  fun toString France  = "France"
    | toString Germany = "Germany"
    | toString Spain   = "Spain"
end


(* === MAIN === *)
structure Main = struct

  datatype 'a wrap = func of 'a -> 'a

  open Flag
  open Country

  fun toRed (Blue   n) = Red n
    | toRed (White  n) = Red n
    | toRed (Red    n) = Red n
    | toRed (Black  n) = Red n
    | toRed (Yellow n) = Red n

  structure doNothing = struct
    fun id x = x
    val nothing = {raw = id, wrapped = func id}
    fun something bla = Int.toString (!bla)
  end

  structure N = doNothing

  val {raw, wrapped} = N.nothing

  val country = France
  val {col1, col2, col3} = flag
  val flag' = {col1 = toRed col1, col2 = toRed col2, col3 = toRed col3}
  val new = (country, flag')

  val x = raw (#1 new)
  val y = raw (#2 new)

end;
