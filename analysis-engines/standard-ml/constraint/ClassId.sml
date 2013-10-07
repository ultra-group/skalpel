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
 *)

(** Defines the structure ClassId which has the opaque constraint on signature refstruct{CLASSID}, this is the structure to deal with identifiers' classes. *)
structure ClassId :> CLASSID = struct

structure L  = LongId
structure EH = ErrorHandler

(** A class variable is typed to be an integer. *)
type classvar   = int

(** Datatype definition for value identifiers, with 13 constructors.
 * Has the following constructors:
 * \arg VAL Undefined status.
 * \arg PAT Non applied identifier in patten - generated at unification when it turns out binding is an accessor.
 * \arg REC Value variable.
 * \arg VRC Constrained to be a value variable.
 * \arg CO0 0-ary constructor.
 * \arg CO1 1-ary constructor.
 * \arg CON Constructor.
 * \arg DA0 0-ary datatype constructor.
 * \arg DA1 1-ary datatype constructor.
 * \arg DAT Datatype constructor.
 * \arg EX0 0-ary exception.
 * \arg EX1 1-ary exception.
 * \arg EXC Exception. *)
datatype vid    = VAL | PAT | REC | VRC | CO0 | CO1| CON | DA0 | DA1 | DAT | EX0 | EX1 | EXC

(** Datatype definition for classes, with 9 constructors.
 * \arg VID Values (of type #vid).
 * \arg TYCON For type constructors.
 * \arg TYVAR For explicit type variables.
 * \arg SIG For signatures.
 * \arg STR For structures.
 * \arg FUNC For functors.
 * \arg OC Overloading classes.
 * \arg CLVAR Class variables (of type #classvar).
 * \arg ANY Unknown status - generated at constraint filtering. *)
datatype class  = VID of vid | TYCON | TYVAR | SIG | STR | FUNC | OC | CLVAR of classvar | ANY

(** Prints an option value using a function it takes as argument if option is not NONE. *)
fun printOp NONE     _ = "-"
  | printOp (SOME x) f = f x

(** Prints a boolean option value. *)
fun printBoolOp x = printOp x Bool.toString

(** Prints a label using #Label.toString. *)
fun printForceStatus x = printOp x L.toString

(** Print class variable. *)
fun printClassVar cv = Int.toString cv

(** Calls printOp on a class variable using #printClassVar. *)
fun printClassVarOp x = printOp x printClassVar

(** Prints #vid constructors. *)
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

(** Prints #class constructors. *)
fun printClass (VID   vid) = "VID(" ^ printVid vid ^ ")"
  | printClass TYCON       = "TYCON"
  | printClass TYVAR       = "TYVAR"
  | printClass SIG         = "SIG"
  | printClass STR         = "STR"
  | printClass FUNC        = "FUNC"
  | printClass OC          = "OC"
  | printClass (CLVAR cv)  = "CLVAR(" ^ printClassVar cv ^ ")"
  | printClass ANY         = "ANY"


(** Prints out a #class value. *)
fun toString class = printClass class

(** A dummy class value - set to CLVAR 0. *)
val dummyClass    = CLVAR 0

(** A ref cell used to hold the next class variable, initially set to 1. *)
val nextClVar     = ref 1

(** Sets the next class variable to a specified value. *)
fun setNext    n  = nextClVar := n

(** Gets the next class variable using the #nextClVar value. *)
fun getClVar   () = !nextClVar

(** Generates fresh class variables. *)
fun freshClVar () = let val x = !nextClVar in nextClVar := x + 1; x end

(** Resets the next class variable to 0. *)
fun resetClVar () = setNext 0

(** Generates a new class variable. *)
fun newClassVar () = CLVAR (freshClVar ())

(* Turns a #classvar into an integer. *)
fun classvarToInt classvar = classvar

(** Checks a #vid is a VAL. *)
fun vidIsVAL VAL = true
  | vidIsVAL _   = false

(** Checks a #vid is a PAT. *)
fun vidIsPAT PAT = true
  | vidIsPAT _   = false

(** Checks a #vid is a DA0. *)
fun vidIsDA0 DA0 = true
  | vidIsDA0 _   = false

(** Checks a #vid is a REC. *)
fun vidIsREC REC = true
  | vidIsREC _   = false

(** Checks a #vid is a VRC. *)
fun vidIsVRC VRC = true
  | vidIsVRC _   = false

(** Checks a #vid is a EX0. *)
fun vidIsEX0 EX0 = true
  | vidIsEX0 _   = false

(** Checks the vid of a #class value is a VAL using #vidIsVAL. *)
fun classIsVAL (VID vid) = vidIsVAL vid
  | classIsVAL _         = false
(** Checks the vid of a #class value is a PAT using #vidIsPAT. *)
fun classIsPAT (VID vid) = vidIsPAT vid
  | classIsPAT _         = false
(** Checks the vid of a #class value is a REC using #vidIsREC. *)
fun classIsREC (VID vid) = vidIsREC vid
  | classIsREC _         = false
(** Checks the vid of a #class value is a VRC using #vidIsVRC. *)
fun classIsVRC (VID vid) = vidIsVRC vid
  | classIsVRC _         = false
(** Checks the vid of a #class value is a vidIsDA0 using #vidIsDA0. *)
fun classIsDA0 (VID vid) = vidIsDA0 vid
  | classIsDA0 _         = false
(** Checks the vid of a #class value is a vidIsEX0 using #vidIsEX0. *)
fun classIsEX0 (VID vid) = vidIsEX0 vid
  | classIsEX0 _         = false

(** Checks a #vid value is some kind of datatype constructor. *)
fun vidIsDAT DAT = true
  | vidIsDAT DA0 = true
  | vidIsDAT DA1 = true
  | vidIsDAT _   = false

(** Checks a #vid value is some kind of exception constructor. *)
fun vidIsEXC EXC = true
  | vidIsEXC EX0 = true
  | vidIsEXC EX1 = true
  | vidIsEXC _   = false

(** Checks a #vid value is a datatype constructor using #vidIsDAT *)
fun classIsDAT (VID vid) = vidIsDAT vid
  | classIsDAT _         = false

(** Checks a #vid value is an exception constructor using #vidIsEXC *)
fun classIsEXC (VID vid) = vidIsEXC vid
  | classIsEXC _         = false

(** Checks a #vid value is some kind of constructor by checking for CO0, CO1, CON, and using #vidIsDAT and #vidIsEXC *)
fun classIsCON (VID CO0) = true
  | classIsCON (VID CO1) = true
  | classIsCON (VID CON) = true
  | classIsCON class     = classIsDAT class orelse classIsEXC class

(** Returns true if a #vid value is a VID, false otherwise. *)
fun classIsVID (VID _)   = true
  | classIsVID _         = false
(** Returns true if a #vid value is a TYCON, false otherwise. *)
fun classIsTYCON TYCON   = true
  | classIsTYCON _       = false
(** Returns true if a #vid value is a SIG, false otherwise. *)
fun classIsSIG SIG       = true
  | classIsSIG _         = false
(** Returns true if a #vid value is a STR, false otherwise. *)
fun classIsSTR STR       = true
  | classIsSTR _         = false
(** Returns true if a #vid value is a FUNC, false otherwise. *)
fun classIsFUNC FUNC     = true
  | classIsFUNC _        = false
(** Returns true if a #vid value is an OC, false otherwise. *)
fun classIsOC OC         = true
  | classIsOC _          = false
(** Returns true if a #vid value is an ANY, false otherwise. *)
fun classIsANY ANY       = true
  | classIsANY _         = false
(** Returns true if a #vid value is a VAR, false otherwise. *)
fun classIsVAR (CLVAR _) = true
  | classIsVAR _         = false

(** Constructs a #class value as a VID with a VAL argument. *)
fun consVAL   () = VID VAL
(** Constructs a #class value as a VID with a PAT argument. *)
fun consPAT   () = VID PAT
(** Constructs a #class value as a VID with a REC argument. *)
fun consREC   () = VID REC
(** Constructs a #class value as a VID with a VRC argument. *)
fun consVRC   () = VID VRC
(** Constructs a #class value as a VID with a CO0 argument. *)
fun consCO0   () = VID CO0
(** Constructs a #class value as a VID with a CO1 argument. *)
fun consCO1   () = VID CO1
(** Constructs a #class value as a VID with a CON argument. *)
fun consCON   () = VID CON
(** Constructs a #class value as a VID with a DA0 argument. *)
fun consDA0   () = VID DA0
(** Constructs a #class value as a VID with a DA1 argument. *)
fun consDA1   () = VID DA1
(** Constructs a #class value as a VID with a DAT argument. *)
fun consDAT   () = VID DAT
(** Constructs a #class value as a VID with an EX0 argument. *)
fun consEX0   () = VID EX0
(** Constructs a #class value as a VID with an EX1 argument. *)
fun consEX1   () = VID EX1
(** Constructs a #class value as a VID with an EXC argument. *)
fun consEXC   () = VID EXC
(** Constructs a #class value as a VID with an ANY argument. *)
fun consANY   () = ANY
(** Constructs a #class value with a TYCON. *)
fun consTYCON () = TYCON
(** Constructs a #class value with a TYVAR. *)
fun consTYVAR () = TYVAR
(** Constructs a #class value with a STR. *)
fun consSTR   () = STR
(** Constructs a #class value with a SIG. *)
fun consSIG   () = SIG
(** Constructs a #class value with a OC. *)
fun consOC    () = OC
(** Constructs a #class value with a FUNC. *)
fun consFUNC  () = FUNC


(** Tuns a VAL constructor of a #vid value into a PAT. *)
fun vidToPAT VAL = PAT
  | vidToPAT _   = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #class value into a PAT. *)
fun classToPAT (VID vid) = (VID (vidToPAT vid))
  | classToPAT _         = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #vid value into a DAT. *)
fun vidToDAT VAL = DAT
  | vidToDAT _   = raise EH.DeadBranch ""

(** Turns a VAL constructor of a #class value into a DAT. *)
fun classToDAT (VID vid) = (VID (vidToDAT vid))
  | classToDAT _ = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #vid value into a DA0. *)
fun vidToDA0 VAL = DA0
  | vidToDA0 _   = raise EH.DeadBranch ""

(** Turns a VAL constructor of a #class value into a DA0. *)
fun classToDA0 (VID vid) = (VID (vidToDA0 vid))
  | classToDA0 _ = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #vid value into a DA1. *)
fun vidToDA1 VAL = DA1
  | vidToDA1 _   = raise EH.DeadBranch ""

(** Turns a VAL constructor of a #class value into a DA1. *)
fun classToDA1 (VID vid) = (VID (vidToDA1 vid))
  | classToDA1 _ = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #vid value into an EX0. *)
fun vidToEX0 VAL = EX0
  | vidToEX0 _   = raise EH.DeadBranch ""

(** Turns a VAL constructor of a #class value into a EX0. *)
fun classToEX0 (VID vid) = (VID (vidToEX0 vid))
  | classToEX0 _         = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #vid value into an EX1. *)
fun vidToEX1 VAL = EX1
  | vidToEX1 _   = raise EH.DeadBranch ""

(** Turns a VAL constructor of a #class value into a EX1. *)
fun classToEX1 (VID vid) = (VID (vidToEX1 vid))
  | classToEX1 _         = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #vid value into a REC. *)
fun vidToREC VAL = REC
  | vidToREC PAT = REC
  | vidToREC _   = raise EH.DeadBranch ""

(** Turns a VAL constructor of a #class value into a REC. *)
fun classToREC (VID vid) = (VID (vidToREC vid))
  | classToREC _ = raise EH.DeadBranch ""

(** Tuns a VAL constructor of a #vid value into a VRC. *)
fun vidToVRC VAL = VRC
  | vidToVRC PAT = VRC
  | vidToVRC _   = raise EH.DeadBranch ""

(** Turns a VAL constructor of a #class value into a VRC. *)
fun classToVRC (VID vid) = (VID (vidToVRC vid))
  | classToVRC _ = raise EH.DeadBranch ""

(** Returns the ANY constructor. *)
fun classToANY _ = ANY

end
