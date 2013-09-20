(*
* Write a function 'nth' that will take as input a list and an integer N and
* return the Nth element from the list.
*
* For example:
*
* nth [66,22,77,558,425] 3
*
* Should return:
*
* 77
*
* Please note you should not use List.nth or List.length to accomplish this task.
*)


fun nth


(********** Do not edit code beneath this line **********)

(* prints a list of integer values *)
fun printList []     = ""
  | printList (h::h2::t) = (Int.toString h) ^ "," ^ (printList (h2::t))
  | printList (h::t) = (Int.toString h)

fun checkAnswers f [] = print("complete")
  | checkAnswers f (((integerList,intToTake),answer)::t) =
    let
	val returnedValue = f integerList intToTake
    in
	if returnedValue  <> answer
	then print ("Test failed with first argument of [" ^ (printList integerList) ^
		    "] and second argument " ^ (Int.toString intToTake) ^ ". Answer should be: [" ^
		    (Int.toString answer) ^ "] but got: [" ^ (Int.toString returnedValue) ^ "].")
	else checkAnswers f t
    end

val tests = [ (([1,1,2,3,5,8],1),1),
	      (([1,1,1,1],1),1),
	      (([~1,~2,~3],3),~3),
	      (([100,2000,30000,4],4),4)
	    ];

checkAnswers nth tests;
