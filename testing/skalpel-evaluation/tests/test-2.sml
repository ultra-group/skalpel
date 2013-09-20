(*
* Write a function "fib" that will take as an input an integer and return the corresponding
* Fibonaci number. The Fibonacci sequence is defined as follows:
*
* fib(0) = 0
* fib(1) = 1
* fib(n) = fib(n-1) + fib(n-2)
*
* For example,
*
* fib 7
*
* should return:
*
* 13
*)

fun fib


(********** Do not edit code beneath this line **********)

fun checkAnswers f [] = print("complete")
  | checkAnswers f ((question,answer)::t) =
    let
	val returnedValue = f question
    in
	if returnedValue  <> answer
	then print ("Test failed with first argument of " ^ (Int.toString question) ^
		    ". Answer should be: " ^ (Int.toString answer) ^ " but got: " ^
		    (Int.toString returnedValue) ^ ".")
	else checkAnswers f t
    end

val tests = [ (0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (6, 8), (7, 13), (8, 21),
	      (9, 34), (10, 55), (11, 89), (12, 144), (13, 233)];

checkAnswers fib tests;

