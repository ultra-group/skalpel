(*
* Function: remove_element
* Argument One: A list of integers
* Argument Two: An integer
* Return type: A list of integers
*
* Write a function 'remove_element' which will remove the integer
* specified in the second argument from the list given as the first
* argument. For example,
*
* remove_element [1,1,2,3,5,8,13] 1
*
* should return the list:
*
* [2,3,5,8,13]
*
*)

fun remove_element



(********** Do not edit code beneath this line **********)

(* prints a list of integer values *)
fun printList []     = ""
  | printList (h::h2::t) = (Int.toString h) ^ "," ^ (printList (h2::t))
  | printList (h::t) = (Int.toString h)

fun checkAnswers f [] = print("complete")
  | checkAnswers f (((integerList,intToRemove),answer)::t) =
    let
	val returnedList = f integerList intToRemove
    in
	if returnedList  <> answer
	then print ("Test failed with first argument of [" ^ (printList integerList) ^
		    "] and second argument " ^ (Int.toString intToRemove) ^ ". Answer should be: [" ^
		    (printList answer) ^ "] but got: [" ^ printList(returnedList) ^ "].")
	else checkAnswers f t
    end

val tests = [ (([1,1,2,3,5,8],1),[2,3,5,8]),
	      (([1,1,1,1],1),[]),
	      (([~1,~2,~3],~4),[~1,~2,~3]),
	      (([100,2000,30000,4],16),[100,2000,30000,4])
	    ];

checkAnswers remove_element tests;

