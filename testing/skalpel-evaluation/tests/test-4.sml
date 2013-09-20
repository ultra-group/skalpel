(*
* Write a function 'dom' which takes as input a list of tuples
* and returns a list of the left-hand elements.
*
* For example,
*
* dom [(1,2), (5,1), (12,68)]
*
* should return:
*
* [1, 5, 12]
*
*)

fun dom



(********** Do not edit code beneath this line **********)

(* prints a list of integer values *)
fun printList []     = ""
  | printList (h::h2::t) = (Int.toString h) ^ "," ^ (printList (h2::t))
  | printList (h::t) = (Int.toString h)

fun printTupleList [] = ""
  | printTupleList ((a,b)::h2::t) = "(" ^ (Int.toString a) ^ "," ^ (Int.toString b) ^ ")" ^ ", " ^ printTupleList (h2::t)
  | printTupleList ((a,b)::t) = "(" ^ (Int.toString a) ^ "," ^ (Int.toString b) ^ ")"


fun checkAnswers f [] = print("complete")
  | checkAnswers f ((integerList,answer)::t) =
    let
	val returnedList = f integerList
    in
	if returnedList  <> answer
	then print ("Test failed with first argument of [" ^ (printTupleList integerList) ^
		    "]. Answer should be: [" ^ (printList answer) ^ "] but got: [" ^
		    printList(returnedList) ^ "].")
	else checkAnswers f t
    end

val tests = [ ([(1,2), (5,1), (12,68)], [1, 5, 12]),
	      ([(6,11)], [6]),
              ([], [])
	    ];

checkAnswers dom tests;
