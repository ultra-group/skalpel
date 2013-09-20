(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o File name:   ClassId.sml
 *  o Description: Defines the structure ClassId which has the signature
 *      CLASSID.  This is the structure to deal with identifiers' classes.
 *)


structure ClassId :> CLASSID = struct

(* STRUCTURES *)

structure L  = LongId
(*structure M  = SplayMapFn(OrdId)*)
structure EH = ErrorHandler

(* DATATYPES AND TYPES *)

(*type 'a map     = 'a M.map*)

(* class variable is typed to be an integer *)
type classvar   = int

(* datatype definition for value identifiers *)
datatype vid    = VAL
		| PAT
		| REC
		| VRC
		| CO0
		| CO1
		| CON
		| DA0
		| DA1
		| DAT
		| EX0
		| EX1
		| EXC

(* datatype definition for classes *)
datatype class  = VID of vid
		| TYCON
		| TYVAR
		| SIG
		| STR
		| FUNC
		| OC  (* WTF is an OC? *)
		| CLVAR of classvar
		| ANY


(* PRINTING SECTION *)

fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"
(*fun printmapgen  xs f = "[" ^ #1 (M.foldri (fn (id, t, (s, c)) =>
					       ("(" ^ I.printId id ^ "," ^ f t ^ ")" ^ c ^ s, ","))
					   ("", "")
					   xs) ^ "]"*)

fun printOp NONE     _ = "-"
  | printOp (SOME x) f = f x

fun printBoolOp x = printOp x Bool.toString

fun printForceStatus x = printOp x L.toString

(* print class variable *)
fun printClassVar cv = Int.toString cv

fun printClassVarOp x = printOp x printClassVar

(* printns different value identifiers *)
fun printVid VAL = "VAL"
  | printVid PAT = "PAT"
  | printVid REC = "REC"
  | printVid VRC = "VRC"
  | printVid CO0 = "CO0"
  | printVid CO1 = "CO1"
  | printVid CON = "CON"
  | printVid DA0 = "DA0"
  | printVid DA1 = "DA1"
  | printVid DAT = "DAT"
  | printVid EX0 = "EX0"
  | printVid EX1 = "EX1"
  | printVid EXC = "EXC"

(* printing for class *)
fun printClass (VID   vid) = "VID(" ^ printVid vid ^ ")"
  | printClass TYCON       = "TYCON"
  | printClass TYVAR       = "TYVAR"
  | printClass SIG         = "SIG"
  | printClass STR         = "STR"
  | printClass FUNC        = "FUNC"
  | printClass OC          = "OC"
  | printClass (CLVAR cv)  = "CLVAR(" ^ printClassVar cv ^ ")"
  | printClass ANY         = "ANY"

fun toString class = printClass class

(* dummy and next values *)
val dummyClass    = CLVAR 0
val nextClVar     = ref 1

(* set and get for class variables *)
fun setNext    n  = nextClVar := n
fun getClVar   () = !nextClVar

(* generating fresh class variables *)
fun freshClVar () = let val x = !nextClVar in nextClVar := x + 1; x end
fun resetClVar () = setNext 0

(* generates a newclass variable *)
fun newClassVar () = CLVAR (freshClVar ())

(* classvar -> int *)
fun classvarToInt classvar = classvar

(**)

(* check style of value identifiers *)
fun vidIsVAL VAL = true
  | vidIsVAL _   = false
fun vidIsPAT PAT = true
  | vidIsPAT _   = false
fun vidIsDA0 DA0 = true
  | vidIsDA0 _   = false
fun vidIsREC REC = true
  | vidIsREC _   = false
fun vidIsVRC VRC = true
  | vidIsVRC _   = false
fun vidIsEX0 EX0 = true
  | vidIsEX0 _   = false

(* checks style of classes *)
fun classIsVAL (VID vid) = vidIsVAL vid
  | classIsVAL _         = false
fun classIsPAT (VID vid) = vidIsPAT vid
  | classIsPAT _         = false
fun classIsREC (VID vid) = vidIsREC vid
  | classIsREC _         = false
fun classIsVRC (VID vid) = vidIsVRC vid
  | classIsVRC _         = false
fun classIsDA0 (VID vid) = vidIsDA0 vid
  | classIsDA0 _         = false
fun classIsEX0 (VID vid) = vidIsEX0 vid
  | classIsEX0 _         = false

(* more styles of value identifiers *)
fun vidIsDAT DAT = true
  | vidIsDAT DA0 = true
  | vidIsDAT DA1 = true
  | vidIsDAT _   = false

fun vidIsEXC EXC = true
  | vidIsEXC EX0 = true
  | vidIsEXC EX1 = true
  | vidIsEXC _   = false

(* more styles of classes *)
fun classIsDAT (VID vid) = vidIsDAT vid
  | classIsDAT _         = false
fun classIsEXC (VID vid) = vidIsEXC vid
  | classIsEXC _         = false
fun classIsCON (VID CO0) = true
  | classIsCON (VID CO1) = true
  | classIsCON (VID CON) = true
  | classIsCON class     = classIsDAT class orelse classIsEXC class

fun classIsVID (VID _)   = true
  | classIsVID _         = false
fun classIsTYCON TYCON   = true
  | classIsTYCON _       = false
fun classIsSIG SIG       = true
  | classIsSIG _         = false
fun classIsSTR STR       = true
  | classIsSTR _         = false
fun classIsFUNC FUNC     = true
  | classIsFUNC _        = false
fun classIsOC OC         = true
  | classIsOC _          = false
fun classIsANY ANY       = true
  | classIsANY _         = false
fun classIsVAR (CLVAR _) = true
  | classIsVAR _         = false

(* constructors *)
fun consVAL   () = VID VAL
fun consPAT   () = VID PAT
fun consREC   () = VID REC
fun consVRC   () = VID VRC
fun consCO0   () = VID CO0
fun consCO1   () = VID CO1
fun consCON   () = VID CON
fun consDA0   () = VID DA0
fun consDA1   () = VID DA1
fun consDAT   () = VID DAT
fun consEX0   () = VID EX0
fun consEX1   () = VID EX1
fun consEXC   () = VID EXC
fun consANY   () = ANY
fun consTYCON () = TYCON
fun consTYVAR () = TYVAR
fun consSTR   () = STR
fun consSIG   () = SIG
fun consOC    () = OC
fun consFUNC  () = FUNC

(* TRANSFORMATIONS ON THE CLASSES *)

fun vidToPAT VAL = PAT
  | vidToPAT _   = raise EH.DeadBranch ""
fun classToPAT (VID vid) = (VID (vidToPAT vid))
  | classToPAT _         = raise EH.DeadBranch ""

fun vidToDAT VAL = DAT
  | vidToDAT _   = raise EH.DeadBranch ""
fun classToDAT (VID vid) = (VID (vidToDAT vid))
  | classToDAT _ = raise EH.DeadBranch ""

fun vidToDA0 VAL = DA0
  | vidToDA0 _   = raise EH.DeadBranch ""
fun classToDA0 (VID vid) = (VID (vidToDA0 vid))
  | classToDA0 _ = raise EH.DeadBranch ""

fun vidToDA1 VAL = DA1
  | vidToDA1 _   = raise EH.DeadBranch ""
fun classToDA1 (VID vid) = (VID (vidToDA1 vid))
  | classToDA1 _ = raise EH.DeadBranch ""

fun vidToEX0 VAL = EX0
  | vidToEX0 _   = raise EH.DeadBranch ""
fun classToEX0 (VID vid) = (VID (vidToEX0 vid))
  | classToEX0 _         = raise EH.DeadBranch ""

fun vidToEX1 VAL = EX1
  | vidToEX1 _   = raise EH.DeadBranch ""
fun classToEX1 (VID vid) = (VID (vidToEX1 vid))
  | classToEX1 _         = raise EH.DeadBranch ""

fun vidToREC VAL = REC
  | vidToREC PAT = REC
  | vidToREC _   = raise EH.DeadBranch ""
fun classToREC (VID vid) = (VID (vidToREC vid))
  | classToREC _ = raise EH.DeadBranch ""

fun vidToVRC VAL = VRC
  | vidToVRC PAT = VRC
  | vidToVRC _   = raise EH.DeadBranch ""
fun classToVRC (VID vid) = (VID (vidToVRC vid))
  | classToVRC _ = raise EH.DeadBranch ""

fun classToANY _ = ANY

end
