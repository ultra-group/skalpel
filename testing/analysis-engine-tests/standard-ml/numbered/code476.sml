(* old test case name: code476.sml *)

(* Example to use with the basis because of the use of 'print' *)
signature STUDENT = sig
    type name
    val name : name
    val toString : unit -> string
end

structure X = struct
  type name = string
  val age  = 26
  val name = "Vincent"
  val dob  = {day = 23, month = 11, year = 1983}
  fun toString () = "---" ^ name ^ "---\n"
end

structure Student = X :> STUDENT

val a = Student.name

signature ASIG = sig
    val toString : unit -> string
end

functor Academy (S : STUDENT) :> ASIG = struct
  val name      = S.name
  val toString  = S.toString
  val institute = "Heriot-Watt"
end

structure AStudentInTheAcademy = Academy (Student);

(* Untypable because name is not in ASIG,
 * but the code would be typable without the next line. *)
val b = AStudentInTheAcademy.name;

val _ = print (AStudentInTheAcademy.toString ());
