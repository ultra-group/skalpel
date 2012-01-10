signature STUDENT =
sig

    datatype grade = A | B | C | D | E | F
    type student = {lastName : string, course : string, grade : grade, year : int, class : string, phone : string}

    val top_students        : student list -> grade * student list
    val students_with_grade : student list -> grade -> student list
    val student_list        : student list

end


structure Student :> STUDENT =
struct

datatype grade = A | B | C | D | E | F
type student = {lastName : string, course : string, grade : grade, year : int, class : string, phone : string}

(* Create some students *)
val student1 =  {lastName="Bloggs"    , course="Computer Science"     , grade=D, year=1, class="A", phone="0131 000 001"}
val student2 =  {lastName="Simmons"   , course="Maths"                , grade=C, year=2, class="B", phone="0131 000 002"}
val student3 =  {lastName="Ferguson"  , course="Management"           , grade=F, year=4, class="B", phone="0131 000 003"}
val student4 =  {lastName="Boyd"      , course="Computer Science"     , grade=B, year=1, class="A", phone="0131 000 004"}
val student5 =  {lastName="Miller"    , course="Chemistry"            , grade=A, year=3, class="B", phone="0131 000 005"}
val student6 =  {lastName="Kerr"      , course="Physics"              , grade=C, year=4, class="A", phone="0131 000 006"}
val student7 =  {lastName="Davis"     , course="Maths"                , grade=B, year=2, class="A", phone="0131 000 007"}
val student8 =  {lastName="Taylor"    , course="Management"           , grade=A, year=1, class="A", phone="0131 000 008"}
val student9 =  {lastName="Miller"    , course="Chemistry"            , grade=A, year=3, class="B", phone="0131 000 009"}
val student10 = {lastName="Kerr"      , course="Physics"              , grade=C, year=4, class="A", phone="0131 000 010"}
val student11 = {lastName="Davis"     , course="Maths"                , grade=B, year=2, class="A", phone="0131 000 011"}
val student12 = {lastName="Taylor"    , course="Management"           , grade=A, year=1, class="A", phone="0131 000 012"}
val student13 = {lastName="Miller"    , course="Chemistry"            , grade=A, year=3, class="B", phone="0131 000 013"}
val student14 = {lastName="Kerr"      , course="Physics"              , grade=C, year=4, class="A", phone="0131 000 014"}
val student15 = {lastName="Taylor"    , course="Management"           , grade=A, year=1, class="A", phone="0131 000 015"}

(* Create a list of students from the studetns created above *)
val student_list = [student1, student2, student3, student4, student5, student6, student7, student8, student9, student10, student11, student12, student13, student14, student15]

(* This function associates a numerical value with the grades a student can be awarded *)
fun assoc_weight_grade x = (fn A => 1 | B => 2 | C => 3 | D => 4 | E => 5 | F => 6) x
(* This funcion returns a list of students who have the best grade out of a *)
(* list of students, as well as returning the grade that they achievd.      *)
fun best_grade []                      clever_students grade = (grade, clever_students)
  | best_grade (students:student list) clever_students grade = let val first         = hd students
								   val rest          = tl students
								   val student_grade = #grade first
							       in
								   if assoc_weight_grade student_grade < assoc_weight_grade grade
								   then best_grade rest [#lastName first] student_grade
								   else
								       if student_grade = grade
								       then best_grade rest (clever_students@[#lastName first]) grade
								       else best_grade rest clever_students grade
							       end;

(* This function returns a list of students with a certain grade *)
fun with_grade []                      students_with_grade _     = students_with_grade
  | with_grade (students:student list) students_with_grade grade = let val first         = hd students
								       val rest          = tl students
								       val student_grade = #grade first
								   in
								       if student_grade = grade
								       then with_grade rest (students_with_grade@[#lastName first]) grade
								       else with_grade rest students_with_grade grade
								   end

(* This funcion returns a list of students who have the best grade out of a *)
(* list of students, as well as returning the grade that they achievd. *)
(* It is basically a user friendly interface to the asdbest_grade function  *)
fun top_students students : grade * student list = best_grade students [] F;

(* This function returns a list of students with a certain grade *)
(* It is basically a user friendly interface to the with_grade function *)
fun students_with_grade students grade = with_grade students [] grade;

end
