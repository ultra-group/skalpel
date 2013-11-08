(* Copyright 2009 2010 Heriot-Watt University
 *
 * Skalpel is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Skalpel is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Infix.sml
 *)

(** Defines the structure Infix which has signature INFIX and is used to deal with infix operators at parsing. *)
structure Infix :> INFIX = struct

structure R  = Reg
structure L  = Label
structure EH = ErrorHandler
structure MS = SplayMapFn(OrdStr)

(** A type and datatype declarations *)
type 'a pack = 'a * R.region

(** A string * region *)
type packstr = string pack

datatype 'a tree = L of 'a pack
		 | O of packstr
		 | N of packstr * 'a tree * 'a tree

(** To represent whether something is left or right associative *)
datatype assoc = LEFT | RIGHT

(** To hold precedence information, an integer. *)
type prec = int

(** Fixity specification is made up of whether the infix function is
 * left associative or right associative and the prescedence of that
 * operator *)
type fixity = assoc * prec

(** An operator is a label with a fixity. *)
type operators = (L.label * fixity MS.map) list ref


(* For now, we don't associate associativity with the operators,
 * it depends on the precedence.
 * The label that goes with the id is the label corresponding to the (*  *)
 * scope of the fixity declaration.  For example, if an id x is
 * declared an infix in a let expression labelled l, then we will
 * add (x, l) in one of the list. *)

(** Initial operators with precedence 7. *)
val initOperators7 = [("div", LEFT,  7), ("mod", LEFT,  7), ("*", LEFT, 7), ("/", LEFT, 7)]
(** Initial operators with precedence 6. *)
val initOperators6 = [("+",   LEFT,  6), ("-",   LEFT,  6), ("^", LEFT, 6)]
(** Initial operators with precedence 5. *)
val initOperators5 = [("::",  RIGHT, 5), ("@",   RIGHT, 5)]
(** Initial operators with precedence 4. *)
val initOperators4 = [("=",   LEFT,  4), ("<>",  LEFT,  4), (">", LEFT, 4), ("<", LEFT, 4), (">=", LEFT, 4), ("<=", LEFT, 4)]
(** Initial operators with precedence 3. *)
val initOperators3 = [(":=",  LEFT,  3), ("o",   LEFT,  3)]
(** Initial operators with precedence 0. *)
val initOperators0 = [("before", LEFT, 0)]

(** Initialise the operators by concatenating the operator lists together *)
val initOperators = initOperators7 @
		    initOperators6 @
		    initOperators5 @
		    initOperators4 @
		    initOperators3 @
		    initOperators0

val initOperatorsMap = foldr (fn ((st, assoc, prec), operators) =>
				 MS.insert (operators, st, (assoc, prec)))
			     MS.empty
			     initOperators

(** Initially a ref cell of a list of a pair of the dummy labels and the #initOperatorsMap. *)
val operators = ref [(L.dummyLab, initOperatorsMap)]

(** Resets the operators *)
fun reset () = operators := [(L.dummyLab, initOperatorsMap)]

(** Returns whether a given parameter is registered as being infix. *)
fun isInfix st = List.exists (fn (lab, map) => Option.isSome (MS.find (map, st))) (!operators)

(** We use foldl because we start with the tighter binding. *)
fun getOperator st = List.foldl (fn ((lab, map), SOME x) => SOME x
				  | ((lab, map), NONE) => MS.find (map, st))
				NONE
				(!operators)

(** Adds a new label to the operators list *)
fun newScope lab = operators := (lab, MS.empty) :: (!operators)

(** Removes a label from the operators list *)
fun rmScope lab =
    case !operators of
	[] => raise EH.DeadBranch "leaving an empty scope?!"
      | ((lab', map) :: operators') =>
	if L.eq lab lab'
	then operators := operators'
	else raise EH.DeadBranch "leaving another scope?!"

(** Adds a new infix operator scope. *)
fun addInfix st assoc prec =
    case !operators of
	[] => raise EH.DeadBranch "cannot add operator to empty scope"
      | ((lab, map) :: operators') =>
	operators := (lab, MS.insert (map, st, (assoc, prec))) :: operators'

(** Add a left precedence operator. *)
fun addInfixL st prec = addInfix st LEFT  prec
(** Add a right precedence operator. *)
fun addInfixR st prec = addInfix st RIGHT prec

(** Remove infixity status of an operator. *)
fun rmInfix st =
    case !operators of
	[] => raise EH.DeadBranch "cannot remove operator from empty scope"
      | ((lab, map) :: operators') =>
	(operators := (lab, #1 (MS.remove (map, st))) :: operators')
	handle LibBase.NotFound => ()


(** Get the region information from a left precedence operator. *)
fun getLeft (L (_, r))    = r
  | getLeft (O (_, r))    = r
  | getLeft (N (_, t, _)) = getLeft t

(** Get the region information from a right precedence operator. *)
fun getRight (L (_, r))    = r
  | getRight (O (_, r))    = r
  | getRight (N (_, t, _)) = getRight t

fun convertGen []     _ _ = raise EH.DeadBranch "DeadBranch84"
  | convertGen [x]    _ _ = [x]
  | convertGen [_, O (str, _)] _ _ = raise EH.DeadBranch ("Infix operator " ^ str ^ " not applied to a pair of terms")
  | convertGen [O (str, _), _] _ _ = raise EH.DeadBranch ("Infix operator " ^ str ^ " not applied to a pair of terms")
  | convertGen [_, _] _ _ = raise EH.DeadBranch "DeadBranch85"
  | convertGen (x :: (z as O (str, r)) :: y :: xs) assoc prec =
    (case getOperator str of
	 NONE => raise EH.DeadBranch (str ^ " should be an infix operator")
       | SOME (assoc', prec') =>
	 if assoc = assoc' andalso prec = prec'
	 then let val node = case assoc of
				 LEFT  => N ((str, r), x, y)
			       | RIGHT => N ((str, r), y, x)
	      in convertGen (node :: xs) assoc prec
	      end
	 else x :: z :: (convertGen (y :: xs) assoc prec))
  | convertGen (x :: (N _) :: y :: xs) _ _ = raise EH.DeadBranch "DeadBranch86"
  | convertGen (x :: (L _) :: y :: xs) _ _ = raise EH.DeadBranch "DeadBranch87"

fun convertGen' tokens prec =
    let val tokens1 = convertGen tokens LEFT prec
	val tokens2 = rev (convertGen (rev tokens1) RIGHT prec)
    in tokens2
    end

(** Pass converting the precedence 9. *)
fun convert9 xs = convertGen' xs 9

(** Pass converting the precedence 8. *)
fun convert8 xs = convertGen' xs 8

(** Pass converting the precedence 7. *)
fun convert7 xs = convertGen' xs 7

(** Pass converting the precedence 6. *)
fun convert6 xs = convertGen' xs 6

(** Pass converting the precedence 5. *)
fun convert5 xs = convertGen' xs 5

(** Pass converting the precedence 4. *)
fun convert4 xs = convertGen' xs 4

(** Pass converting the precedence 3. *)
fun convert3 xs = convertGen' xs 3

(** Pass converting the precedence 2. *)
fun convert2 xs = convertGen' xs 2

(** Pass converting the precedence 1. *)
fun convert1 xs = convertGen' xs 1

(** Pass converting the precedence 0. *)
fun convert0 xs = convertGen' xs 0

(** Compose all of the convert functions and aply to the argument. *)
fun convert' x = (convert0 o
		  convert1 o
		  convert2 o
		  convert3 o
		  convert4 o
		  convert5 o
		  convert6 o
		  convert7 o
		  convert8 o
		  convert9) x

(** Calls convert' on the argument. *)
fun convert xs =
    case convert' xs of
	[x] => x
      | _   => raise EH.DeadBranch "DeadBranch88"


end
