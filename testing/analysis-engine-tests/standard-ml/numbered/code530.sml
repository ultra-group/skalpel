(* old test case name: code530.sml *)

(* Untypable because of the value polymorphism restriction.
 * The error goes through a structure binding. *)

structure Foo = struct
  structure FooBar = struct
  fun null x = x
  end

  datatype ('a, 'b) toto = null of 'a -> 'a

  open FooBar

  structure structureBla = struct
    val doALot = {field1 = fn x => x, field2 = null (fn x => x)}
    fun doMore bla = Int.toString (!bla)
  end

  structure S = structureBla

  val {field1, field2} = S.doALot

  val x = field2 1
  val y = field2 true
end;
