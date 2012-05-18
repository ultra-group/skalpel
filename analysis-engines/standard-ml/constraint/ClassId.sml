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


(*(* VALUES AND FUNCTIONS *)

val emMap     = M.empty
val foldliMap = M.foldli
val mapMap    = M.map
val appi      = M.appi

fun domMap map = I.ord (#1 (ListPair.unzip (M.listItemsi map)))

fun consMap id str map = M.insert (map, id, str)
fun getMap id map = M.find (map, id)

fun tyToMap set = I.foldl (fn (id, map) => consMap id (CONS []) map) emMap set*)

(* class variable *)

(*val dummyClassvar = 0
val nextclassvar = ref 1
fun setnext       n  = nextclassvar := n
fun getclassvar   () = !nextclassvar
fun freshclassvar () = let val x = !nextclassvar in nextclassvar := x + 1; x end
fun resetclassvar () = setnext 0*)


(*(**)

fun genConsI lid  = TYID lid
fun genConsC cons = CONS cons
val emCons = CONS []


(* Sets the classvar option of a class if it's not already done. *)

fun setClassVar (class, NONE) = (class, SOME (freshclassvar ()))
  | setClassVar x = x

fun getClassVar (_, x) = x
fun getPClass   (x, _) = x

fun isClassVar (_, SOME _) = true
  | isClassVar _           = false*)


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

(*fun tyconIsDAT (DAT _) = true
  | tyconIsDAT _       = false
fun tyconIsTYP (TYP _) = true
  | tyconIsTYP _       = false*)

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


(*fun classIsDAT (TYCON tycon, _) = tyconIsDAT tycon
  | classIsDAT _                = false
fun classIsTYP (TYCON tycon, _) = tyconIsTYP tycon
  | classIsTYP _                = false*)

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

(* Checks if class1 can bind class2 *)

(*fun vidCompatible (VAL _) (VAL _) = true
  | vidCompatible (VAL _) (CON _) = false
  | vidCompatible (VAL _) (EXC _) = false
  | vidCompatible (REC _) (VAL _) = true
  | vidCompatible (REC _) (CON _) = false
  | vidCompatible (REC _) (EXC _) = false
  | vidCompatible (CON _) (VAL _) = true
  | vidCompatible (CON _) (CON _) = true
  | vidCompatible (CON _) (EXC _) = false
  | vidCompatible (EXC _) (VAL _) = true
  | vidCompatible (EXC _) (CON _) = true
  | vidCompatible (EXC _) (EXC _) = true
  | vidCompatible AVI      _      = true
  | vidCompatible vid1    vid2    =
    (print ("1st vid: " ^ printVid vid1 ^ " " ^
	    "2nd vid: " ^ printVid vid2);
     raise EH.DeadBranch "")

fun compatible (VID vid1, _) (VID vid2, _) = vidCompatible vid1 vid2
  | compatible (TYCON _, _)  (TYCON _, _)  = true
  | compatible (STR, _)      (STR, _)      = true
  | compatible (SIG, _)      (SIG, _)      = true
  | compatible (FUNC, _)     (FUNC, _)     = true
  | compatible (OC, _)       (OC, _)       = true
  | compatible cl1           cl2           =
    (print ("1st class: " ^ toString cl1 ^ " " ^
	    "2nd class: " ^ toString cl2);
     raise EH.DeadBranch "wrong classes in binding")

(**)

fun newRec labs = (labs, NONE)
fun newRec' lab = (L.singleton lab, NONE)
fun newRecVal lab labs = (L.singleton lab, SOME labs)*)

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

(*fun consREClabs ls  = (VID (REC (newRec ls)), NONE)
fun consDATcons xs  = (TYCON (DAT (T.freshseqvar (), xs)), NONE)
fun consTYPsv   sv  = (TYCON (TYP sv), NONE)
fun consSTRstr  str = (STR, NONE)
fun consSIGstr  str = (SIG, NONE)
fun consVID     vid = (VID vid, NONE)

fun consDATcons' xs  () = consDATcons xs
fun consSTRstr'  str () = consSTRstr str

fun consDAT  () = (TYCON (DAT (T.freshseqvar (), CONS [])), NONE) (* Initialy it's a type function *)

(* Returns the label of the identifier and the labels associated
 * to the name of the function in the different branches of a fun dec
 * if the identifier is a function declared with a fun dec. *)

fun getVidREClabs (REC (labs, _)) =
    if L.isEmpty labs
    then raise EH.DeadBranch ""
    else labs
  | getVidREClabs _ = L.empty
fun getClassREClabs (VID vid, _) = getVidREClabs vid
  | getClassREClabs _ = L.empty


(* Returns the labels responsible for the status of the id *)

fun getClassRECst (VID (REC (labs, rf)), _) = Option.getOpt (rf, labs)
  | getClassRECst _ = raise EH.DeadBranch "wrong class"


(* Checks is the value status of an identifier has been confirmed *)

fun isForcedREC (VID (REC (_, SOME labs)), _) = true (*not (L.isEmpty labs)*)
  | isForcedREC _ = false


(* Returns the labels contained by a CON *)

fun getClassCON (VID (CON x), _) = x
  | getClassCON _ = raise EH.DeadBranch "wrong class"


(* Returns the labels contained by a EXC *)

fun getClassEXC (VID (EXC x), _) = x
  | getClassEXC _ = raise EH.DeadBranch "wrong class"


(* true if no constructor for the type *)
fun isConsDATem (CONS []) = true
  | isConsDATem _ = false

fun isClassDATem (TYCON (DAT (_, cons)), _) = isConsDATem cons
  | isClassDATem _ = false

fun isClassDATcons (TYCON (DAT (_, CONS _)), _) = true
  | isClassDATcons _ = false*)


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

(*fun classToSTRstr (STR (*_*), cvop) str = (STR (*str*), cvop)
  | classToSTRstr _       _   = raise EH.DeadBranch ""

fun classToSIGstr (SIG (*_*), cvop) str = (SIG (*str*), cvop)
  | classToSIGstr _             _   = raise EH.DeadBranch ""


(* Reset the labels of a REC *)

fun setClassREClabs (VID (REC (_, x)), cvop) labs = (VID (REC (labs, x)), cvop)
  | setClassREClabs _ _ = raise EH.DeadBranch "trying to reset a wrong class"


(* returns the labels associated to an as in a pattern that constrain
 * an identifier to be a value variable *)

fun getVidPATval (PAT (labs, _)) = labs
  | getVidPATval _ = L.empty

fun getClassPATval (VID vid, _) = getVidPATval vid
  | getClassPATval _ = L.empty


(* These functions returns the constructors of a datatype and raises
 * an exception if the constrained id is not for a datatype *)

fun getTyconDATcons (DAT (_, xs)) = xs
  | getTyconDATcons _ = raise EH.DeadBranch ""

fun getClassDATcons (TYCON tycon, _) = getTyconDATcons tycon
  | getClassDATcons _ = raise EH.DeadBranch ""

fun getClassDATconsC class =
    let val cons = getClassDATcons class
    in case cons of
	   (CONS xs) => xs
	 | _ => raise EH.DeadBranch ""
    end

fun getClassDATconsI class =
    let val cons = getClassDATconsC class
    in I.ord (map #1 cons)
    end


(* Returns the labels of the constructors of a datatype and raises
 * an exception if the class is not for a datatype *)

fun getTyconDATlabs (DAT (_, cons)) =
    (case cons of
	 (CONS xs) => L.concats (map (fn x => L.ord [#2 x, #3 x]) xs)
       | _ => raise EH.DeadBranch "")
  | getTyconDATlabs _ = raise EH.DeadBranch ""

fun getClassDATlabs (TYCON tycon, _) = getTyconDATlabs tycon
  | getClassDATlabs _ = raise EH.DeadBranch ""


(* Returns the information contained into a REC constraint *)

fun getClassREC (VID (REC x), _) = SOME x
  | getClassREC _                = NONE

(* Returns the info held in a STR/SIG *)

(*fun getClassSTR (STR (*str*), _) = emStr(*str*)
  | getClassSTR _ = raise EH.DeadBranch ""*)

(*fun getClassSIG (SIG (*str*), _) = emStr(*str*)
  | getClassSIG class = (print (toString class); raise EH.DeadBranch "")*)


(* Returns the VID stored into a class *)

fun getClassVID (VID vid, _) = vid
  | getClassVID _ = raise EH.DeadBranch ""


(**)

fun getTYP (TYCON (TYP sv), _) = sv
  | getTYP _ = raise EH.DeadBranch "wrong kind of class"

fun tyconTYP (TYP sv) = [sv]
  | tyconTYP _        = []

fun classTYP [] = []
  | classTYP ((TYCON tycon, _) :: xs) = (tyconTYP tycon) @ (classTYP xs)
  | classTYP (_ :: xs) = classTYP xs


(* returns the sequence variable contained in any of the TYCONs *)

fun tyconTYCONseq (TYP sv)      = sv
  | tyconTYCONseq (DAT (sv, _)) = sv
(*  | tyconTYCONseq (TYC sv)      = sv*)

fun getTYCONseq (TYCON x, _) = tyconTYCONseq x
  | getTYCONseq _ = raise EH.DeadBranch "wrong class"


(* Applies a function to the sequences stored in the TYCONs *)

fun tyconTYCONmapSeq (TYP sv)      f = TYP (f sv)
  | tyconTYCONmapSeq (DAT (sv, x)) f = DAT (f sv, x)
(*  | tyconTYCONmapSeq (TYC sv)      f = TYC (f sv)*)

fun classTYCONmapSeq (TYCON x, cvop) f = (TYCON (tyconTYCONmapSeq x f), cvop)
  | classTYCONmapSeq x _ = x


(**)

fun consDATmapCons (CONS xs) f = CONS (map (fn (id, l1, l2, ty, vid) => (id, l1, l2, f ty, vid)) xs)
  | consDATmapCons x _ = x

fun tyconDATmapCons (DAT (sv, x)) f = DAT (sv, consDATmapCons x f)
  | tyconDATmapCons x _ = x

fun classDATmapCons (TYCON x, cvop) f = (TYCON (tyconDATmapCons x f), cvop)
  | classDATmapCons x _ = x*)

end
