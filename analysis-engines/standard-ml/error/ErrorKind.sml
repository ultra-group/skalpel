(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 *
 *
 * This file is part of the ULTRA SML Type Error Slicer (SMLTES) -
 * a Type Error Slicer for Standard ML written by the ULTRA Group of
 * Heriot-Watt University, Edinburgh.
 *
 * SMLTES is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * SMLTES is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with SMLTES.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   ErrorKind.sml
 *  o Description: Contains the definition of the different kinds of
 *      errors handled by our slicer.  The file defines the structure
 *      ErrorKind which has the signature ERRORKIND.
 *)


structure ErrorKind :> ERRORKIND = struct

(* abbreviate structure names *)
structure T  = Ty
structure I  = Id
structure L  = Label
structure EH = ErrorHandler

(* declare some types (mostly for errors!) *)
type label    = int
type id       = int
type tyname   = int
type laberr   = (label * string) list
type recerr   = laberr * laberr * laberr * laberr
type specerr  = label * id
type arrerr   = label * int
type iderr    = label * id
type idserr   = label * id * string
type tnerr    = label * tyname
type unmerr   = specerr * specerr list * label
type synerr   = (int list * int) option

(* in the Overload errors, need to add the labels associated with the overloading classes. *)
datatype kind = Circularity
	      | Overload       of iderr  * tnerr * tnerr list (* value iderr overloaded to tnerrlist used on tnerr     *)
	      | OverloadCst    of idserr * tnerr * tnerr list (* constant idserr overloaded to tnerrlist used on tnerr *)
	      | OverloadClash  of idserr * tnerr list * idserr * tnerr list (* two constaint constrained to be equal but overloaded to different types *)
	      | OverloadIdCst  of iderr  * tnerr list * idserr * tnerr list (* value iderr overloaded to tnerrlist used on idserr                      *)
              | ArityClash     of arrerr * arrerr
              | TyConsClash    of tnerr  * tnerr
	      | NotGenClash    of iderr  * tnerr
	      | TooGenSig      of iderr  * iderr * label list (* the identifier iderr(1st) is monomorphic because of the label list but its specification contains the variable iderr(2nd) *)
	      | TyFunClash     of iderr  * tnerr
	      | LabTyClash     of recerr
	      | Unmatched      of unmerr
	      (*| UnbIdSig      of unmerr*)
	      | UnbWhere       of unmerr
              | DatTypClash    of id * label * label
	      | ConsArgNApp    of label * label
	      | ConsNArgApp    of label * label
	      | MissConsSig    of (label * id) * (label * id) list
	      | MissConsStr    of (label * id) * (label * id) list
	      | MultiOcc       of synerr
	      | ValVarApp      of synerr
	      | ExcIsVar       of synerr
	      | ExcIsDat       of synerr
	      | ConIsVar       of synerr
	      | DatIsExc       of synerr
	      | TyVarBind      of synerr
	      | Warning        of string
	      | Parsing        of string
	      | NonFlexWhere   of iderr * iderr
	      | IllFormedWhere of iderr * iderr
	      | RigidWhere
	      | Inclusion
	      | AppNotApp
	      | DiffFunName
	      | DiffNbArgFun
	      | FreeTyVarTop
	      | AsPatVar
	      | FnRecExp
	      | RealInPat
	      | FreeIdent
	      | FreeOpen

fun printLab lab = Int.toString lab

fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(* PRINTING FOR LISP *)

fun printasmpop NONE = "-"
  | printasmpop (SOME (nl, n)) = (foldr (fn (x, y) => (Int.toString x) ^ "." ^ y) "" nl) ^ (Int.toString n)

fun getSt n asc = I.printId' (I.fromInt n) asc

fun removeFieldName _ [] = (false, [])
  | removeFieldName (st : string) ((x as (_, st')) :: xs) =
    if st = st'
    then (true, xs)
    else (fn (b, xs') => (b, x :: xs')) (removeFieldName st xs)

fun compareFieldNames [] [] = true
  | compareFieldNames [] _  = false
  | compareFieldNames ((_, st) :: xs) ys =
    let
	val (b, zs) = removeFieldName st ys
    in b andalso compareFieldNames xs zs
    end

fun printFields xs =
    "{" ^
    #1 (foldr
	    (fn ((_, lc), (y, z)) => (lc ^ z ^ y, ","))
	    ("", "")
	    xs) ^
    "}"

fun printLabTyNames [] = ""
  | printLabTyNames [(l, tn)] = T.printtyname' tn
  | printLabTyNames ((l, tn) :: xs) = T.printtyname' tn ^ ", " ^ printLabTyNames xs

fun printErrKind Circularity _ = ("CIR", "Circularity")
  | printErrKind (Overload ((lid, id), (l, tn), ltns)) asc =
    ("OVE",
     "Variable "
     ^ getSt id asc
     ^ " overloaded to a list of types not including "
     (*^ printLabTyNames ltns
      ^ " not defined at type "*)
     ^ T.printtyname' (T.tynameFromInt tn))
  | printErrKind (OverloadCst ((lid, id, str), (l, tn), ltns)) asc =
    ("OVC",
     "Constant "
     ^ str
     ^ " overloaded to the overloading class "
     ^ getSt id asc
     ^  " not including "
     ^ T.printtyname' (T.tynameFromInt tn))
  | printErrKind (OverloadClash ((lid1, id1, str1), ltns1, (lid2, id2, str2), ltns2)) asc =
    ("OCL",
     "Clash between the two constants "
     ^ str1
     ^ " overloaded to the overloading class "
     ^ getSt id1 asc
     ^ " and "
     ^ str2
     ^ " overloaded to the overloading class "
     ^ getSt id2 asc)
  | printErrKind (OverloadIdCst ((lid1, id1), ltns1, (lid2, id2, str), ltns2)) asc =
    ("OIC",
     "Value "
     ^ getSt id1 asc
     ^ " overloaded to a list of types not including any type of "
     ^ str
     ^ " itself overloaded to the overloading class "
     ^ getSt id2 asc)
  | printErrKind (ArityClash ((l1, n1), (l2, n2))) _ =
    ("ARI",
     "Arity clash between "
     ^ Int.toString n1
     ^ " and "
     ^ Int.toString n2)
  | printErrKind (TyConsClash ((l1, tn1), (l2, tn2))) asc =
    ("TYP",
     "Type constructor clash between "
     ^ T.printtyname' (T.tynameFromInt tn1)
     ^ " and "
     ^ T.printtyname' (T.tynameFromInt tn2))
  | printErrKind (NotGenClash ((l1, tv), (l2, tn))) _ =
    ("GEN",
     "Structure's signature too general at type "
     ^ T.printtyname' (T.tynameFromInt tn))
  | printErrKind (TooGenSig ((lab1, id), (lab2, tv), labs)) asc =
    ("TGS",
     "The value identifier "
     ^ getSt id asc
     ^ " has a monomorphic type in structure but the type variable "
     ^ getSt tv asc
     ^ " occurs in its specification")
  | printErrKind (TyFunClash ((l1, tv), (l2, tn))) _ =
    ("TFC",
     "Different type function in structure and its signature")
  | printErrKind (LabTyClash (ll1, ll2, ll3, ll4)) _ =
    ("REC",
     "Record clash, the fields "
     ^ printFields ll1
     ^ " conflict with "
     ^ printFields ll2
     ^ (if compareFieldNames ll3 ll4
	then case ll3 of
		 [] => ""
	       | _ => " (shared fields: " ^ printFields ll3 ^ ") "
	else raise EH.DeadBranch ""))
  | printErrKind (Unmatched ((l, n), ls, _)) asc =
    ("UNM",
     "Unmatched specification "
     ^ getSt n asc
     ^ " not in "
     ^ "{" ^ #1 (foldr (fn ((_, n), (y, z)) => (getSt n asc ^ z ^ y, ",")) ("", "") ls)
     ^ "} from structure")
  | printErrKind (UnbWhere ((l, n), ls, _)) asc =
    ("WHR",
     "Unmatched 'where' clause "
     ^ getSt n asc
     ^ " not in "
     ^ "{" ^ #1 (foldr (fn ((_, n), (y, z)) => (getSt n asc ^ z ^ y, ",")) ("", "") ls)
     ^ "} from signature")
  | printErrKind (MissConsSig ((l, n), ls)) asc =
    ("MSI",
     "Missing constructor in signature "
     ^ getSt n asc
     ^ " not in "
     ^ "{" ^ #1 (foldr (fn ((_, n), (y, z)) => (getSt n asc ^ z ^ y, ",")) ("", "") ls)
     ^ "}")
  | printErrKind (MissConsStr ((l, n), ls)) asc =
    ("MST",
     "Missing constructor in structure "
     ^ getSt n asc
     ^ " not in "
     ^ "{" ^ #1 (foldr (fn ((_, n), (y, z)) => (getSt n asc ^ z ^ y, ",")) ("", "") ls)
     ^ "}")
  | printErrKind (DatTypClash (n, l1, l2)) asc =
    ("DTC",
     getSt n asc
     ^ " is defined as a type in a structure and a datatype in its signature")
  | printErrKind (Warning st) _ = ("WRN", "Warning: " ^ st)
  | printErrKind (Parsing st) _ =
    ("PAR",
     case st of "" => "Parsing problem" | _ => "Parsing problem (" ^ st ^ ")")
  | printErrKind (NonFlexWhere ((lab1, id1), (lab2, id2))) asc =
    ("NFW",
     "The identifier "
     ^ getSt id1 asc
     ^ "'s type has been replaced by a type that is not a type name"
     ^ " and therefore cannot be replaced again")
  | printErrKind (IllFormedWhere ((lab1, id1), (lab2, id2))) asc =
    ("NFW",
     "The identifier "
     ^ getSt id1 asc
     ^ " is specified as a datatype in signature"
     ^ " but its type is replaced by a type that is not a type name")
  | printErrKind RigidWhere      _ = ("WHR", "A where clause is constraining a type name that is part of the context")
  | printErrKind (MultiOcc   eo) _ = ("MUL", "Multi occurrence")
  | printErrKind (ValVarApp  eo) _ = ("VAL", "Applied value variable")
  | printErrKind (ExcIsVar   eo) _ = ("EXV", "Identifier declared as a value and used as an exception")
  | printErrKind (ExcIsDat   eo) _ = ("EXD", "Identifier declared as a datatype constructor and used as an exception")
  | printErrKind (ConIsVar   eo) _ = ("LON", "Identifier declared as a value and used as a constructor")  (*datatype/exception*)
  | printErrKind (DatIsExc   eo) _ = ("DCE", "Identifier declared as an exception and used as a datatype constructor")
  | printErrKind (TyVarBind  eo) _ = ("UNG", "Ungeneralisable bound type variable")
  | printErrKind Inclusion       _ = ("INC", "Unbound type variable in type or datatype declaration")
  | printErrKind AppNotApp       _ = ("APP", "Applied and not applied value")
  | printErrKind (ConsArgNApp _) _ = ("CAN", "Non applied constructor that is defined to take an argument")
  | printErrKind (ConsNArgApp _) _ = ("CNA", "Applied constructor that is defined to take no argument")
  | printErrKind DiffFunName     _ = ("FUN", "Different function name")
  | printErrKind DiffNbArgFun    _ = ("ARG", "Different number of arguments")
  | printErrKind FreeTyVarTop    _ = ("FRE", "Free tyvar at top-level")
  | printErrKind AsPatVar        _ = ("LAS", "Left of 'as' must be a variable")
  | printErrKind FnRecExp        _ = ("FNE", "Expressions within rec value bindings must be functions")
  | printErrKind RealInPat       _ = ("REA", "Reals cannot occur within patterns")
  | printErrKind FreeIdent       _ = ("IDE", "Free identifier")
  | printErrKind FreeOpen        _ = ("IDE", "Free opened or replicated identifier (hidding some context)")

(* SML PRINTING *)

fun printSmlAsmpStr []        = ""
  | printSmlAsmpStr [x]       = Int.toString x
  | printSmlAsmpStr (x :: xs) = Int.toString x ^ "," ^ printSmlAsmpStr xs

fun printSmlAsmpOp NONE           = "NONE"
  | printSmlAsmpOp (SOME (nl, n)) = "SOME ([" ^ printSmlAsmpStr nl ^ "]," ^ Int.toString n ^ ")"

fun printSmlLabErr []              = ""
  | printSmlLabErr [(l, st)]       = "(" ^ printLab l ^ ",\"" ^ st ^ "\")"
  | printSmlLabErr ((l, st) :: xs) = "(" ^ printLab l ^ ",\"" ^ st ^ "\")," ^ printSmlLabErr xs

fun printSmlUnm []             = ""
  | printSmlUnm [(l, n)]       = "(" ^ printLab l ^ "," ^ Int.toString n ^ ")"
  | printSmlUnm ((l, n) :: xs) = "(" ^ printLab l ^ "," ^ Int.toString n ^ ")," ^ printSmlUnm xs

fun printSmlLabTyNames []              = ""
  | printSmlLabTyNames [(l, tn)]       = "(" ^ printLab l ^ "," ^ T.printsmltn (T.tynameFromInt tn) ^ ")"
  | printSmlLabTyNames ((l, tn) :: xs) = "(" ^ printLab l ^ "," ^ T.printsmltn (T.tynameFromInt tn) ^ ")," ^ printSmlLabTyNames xs

val transfun = fn #"\n" => ""
		| #"\\" => "\\\\"
		| #"\"" => "\\\""
		| x     => Char.toString x

fun printSmlErrKind Circularity = "ErrorKind.Circularity"
  | printSmlErrKind (Overload ((id, lid), (l, tn), ltns)) =
    "ErrorKind.Overload("
    ^ "(" ^ printLab id ^ "," ^ Int.toString lid ^ "),"
    ^ "(" ^ printLab l  ^ "," ^ T.printsmltn (T.tynameFromInt tn)  ^ "),"
    ^ "[" ^ printSmlLabTyNames ltns ^ "])"
  | printSmlErrKind (OverloadCst ((id, lid, str), (l, tn), ltns)) =
    "ErrorKind.OverloadCst("
    ^ "(" ^ printLab id ^ "," ^ Int.toString lid ^ ",\"" ^ str ^ "\"" ^ "),"
    ^ "(" ^ printLab l  ^ "," ^ T.printsmltn (T.tynameFromInt tn)  ^ "),"
    ^ "[" ^ printSmlLabTyNames ltns ^ "])"
  | printSmlErrKind (OverloadClash ((id1, lid1, str1), ltns1, (id2, lid2, str2), ltns2)) =
    "ErrorKind.OverloadClash("
    ^ "(" ^ printLab id1 ^ "," ^ Int.toString lid1 ^ ",\"" ^  String.translate transfun str1 ^ "\"" ^ "),"
    ^ "[" ^ printSmlLabTyNames ltns1 ^ "],"
    ^ "(" ^ printLab id2 ^ "," ^ Int.toString lid2 ^ ",\"" ^  String.translate transfun str2 ^ "\"" ^ "),"
    ^ "[" ^ printSmlLabTyNames ltns2 ^ "])"
  | printSmlErrKind (OverloadIdCst ((id1, lid1), ltns1, (id2, lid2, str2), ltns2)) =
    "ErrorKind.OverloadIdCst("
    ^ "(" ^ printLab id1 ^ "," ^ Int.toString lid1 ^ "),"
    ^ "[" ^ printSmlLabTyNames ltns1 ^ "],"
    ^ "(" ^ printLab id2 ^ "," ^ Int.toString lid2 ^ ",\"" ^ String.translate transfun str2 ^ "\"" ^ "),"
    ^ "[" ^ printSmlLabTyNames ltns2 ^ "])"
  | printSmlErrKind (ArityClash ((l1, n1), (l2, n2))) =
    "ErrorKind.ArityClash("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString n1 ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ Int.toString n2 ^ "))"
  | printSmlErrKind (TyConsClash ((l1, tn1), (l2, tn2))) =
    "ErrorKind.TyConsClash("
    ^ "(" ^ printLab l1 ^ "," ^ T.printsmltn (T.tynameFromInt tn1) ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ T.printsmltn (T.tynameFromInt tn2) ^ "))"
  | printSmlErrKind (NotGenClash ((l1, tv), (l2, tn))) =
    "ErrorKind.NotGenClash("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString tv ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ T.printsmltn (T.tynameFromInt tn) ^ "))"
  | printSmlErrKind (TooGenSig ((l1, id), (l2, tv), labs)) =
    "ErrorKind.TooGenSig("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString id ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ Int.toString tv ^ "),"
    ^ printlistgen labs Int.toString ^ ")"
  | printSmlErrKind (TyFunClash ((l1, tv), (l2, tn))) =
    "ErrorKind.TyFunClash("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString tv ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ T.printsmltn (T.tynameFromInt tn) ^ "))"
  | printSmlErrKind (LabTyClash (ll1, ll2, ll3, ll4)) =
    "ErrorKind.LabTyClash("
    ^ "[" ^ printSmlLabErr ll1 ^ "]" ^ "," ^ "[" ^ printSmlLabErr ll2 ^ "]" ^ ","
    ^ "[" ^ printSmlLabErr ll3 ^ "]" ^ "," ^ "[" ^ printSmlLabErr ll4 ^ "]" ^ ")"
  | printSmlErrKind (Unmatched ((l, n), ls, lab)) =
    "ErrorKind.Unmatched("
    ^ "(" ^ printLab l ^ "," ^ Int.toString n ^ ")"
    ^ "," ^ "[" ^ printSmlUnm ls ^ "]"
    ^ "," ^ printLab lab ^ ")"
  | printSmlErrKind (UnbWhere ((l, n), ls, lab)) =
    "ErrorKind.UnbWhere("
    ^ "(" ^ printLab l ^ "," ^ Int.toString n ^ ")"
    ^ "," ^ "[" ^ printSmlUnm ls ^ "]"
    ^ "," ^ printLab lab ^ ")"
  | printSmlErrKind (MissConsSig ((l, n), ls)) =
    "ErrorKind.MissConsSig("
    ^ "(" ^ printLab l ^ "," ^ Int.toString n ^ ")"
    ^ "," ^ "[" ^ printSmlUnm ls ^ "]" ^ ")"
  | printSmlErrKind (MissConsStr ((l, n), ls)) =
    "ErrorKind.MissConsStr("
    ^ "(" ^ printLab l ^ "," ^ Int.toString n ^ ")"
    ^ "," ^ "[" ^ printSmlUnm ls ^ "]" ^ ")"
  | printSmlErrKind (DatTypClash (n, l1, l2)) =
    "ErrorKind.DatTypClash(" ^ Int.toString n ^ "," ^ printLab l1 ^ "," ^ printLab l2 ^ ")"
  | printSmlErrKind (ConsArgNApp (l1, l2)) =
    "ErrorKind.ConsArgNApp(" ^ printLab l1 ^ "," ^ printLab l2 ^ ")"
  | printSmlErrKind (ConsNArgApp (l1, l2)) =
    "ErrorKind.ConsNArgApp(" ^ printLab l1 ^ "," ^ printLab l2 ^ ")"
  | printSmlErrKind (NonFlexWhere ((lab1, id1), (lab2, id2))) =
    "ErrorKind.NonFlexWhere(" ^
    "(" ^ printLab lab1 ^ "," ^ Int.toString id1 ^ ")," ^
    "(" ^ printLab lab2 ^ "," ^ Int.toString id2 ^ "))"
  | printSmlErrKind (IllFormedWhere ((lab1, id1), (lab2, id2))) =
    "ErrorKind.IllFormedWhere(" ^
    "(" ^ printLab lab1 ^ "," ^ Int.toString id1 ^ ")," ^
    "(" ^ printLab lab2 ^ "," ^ Int.toString id2 ^ "))"
  | printSmlErrKind (MultiOcc   eo) = "ErrorKind.MultiOcc("   ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (ValVarApp  eo) = "ErrorKind.ValVarApp("  ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (ExcIsVar   eo) = "ErrorKind.ExcIsVar("   ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (ExcIsDat   eo) = "ErrorKind.ExcIsDat("   ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (ConIsVar   eo) = "ErrorKind.ConIsVar("   ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (DatIsExc   eo) = "ErrorKind.DatIsExc("   ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (TyVarBind  eo) = "ErrorKind.TyVarBind("  ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (Warning    st) = "ErrorKind.Warning(\""  ^ st ^ "\")"
  | printSmlErrKind (Parsing    st) = "ErrorKind.Parsing(\""  ^ st ^ "\")"
  | printSmlErrKind RigidWhere      = "ErrorKind.RigidWhere"
  | printSmlErrKind Inclusion       = "ErrorKind.Inclusion"
  | printSmlErrKind AppNotApp       = "ErrorKind.AppNotApp"
  | printSmlErrKind DiffFunName     = "ErrorKind.DiffFunName"
  | printSmlErrKind DiffNbArgFun    = "ErrorKind.DiffNbArgFun"
  | printSmlErrKind FreeTyVarTop    = "ErrorKind.FreeTyVarTop"
  | printSmlErrKind AsPatVar        = "ErrorKind.AsPatVar"
  | printSmlErrKind FnRecExp        = "ErrorKind.FnRecExp"
  | printSmlErrKind RealInPat       = "ErrorKind.RealInPat"
  | printSmlErrKind FreeIdent       = "ErrorKind.FreeIdent"
  | printSmlErrKind FreeOpen        = "ErrorKind.FreeOpen"


(*--------------------*)


fun issem Circularity        = true
  | issem (Overload       _) = true
  | issem (OverloadCst    _) = true
  | issem (OverloadClash  _) = true
  | issem (OverloadIdCst  _) = true
  | issem (Unmatched      _) = true (* we can't never find a smaller one though *)
  | issem (UnbWhere       _) = true (* same as unmachted *)
  | issem (MissConsSig    _) = true
  | issem (MissConsStr    _) = true
  | issem (ArityClash     _) = true
  | issem (TyConsClash    _) = true
  | issem (NotGenClash    _) = true
  | issem (TooGenSig      _) = true
  | issem (TyFunClash     _) = true
  | issem (LabTyClash     _) = true
  | issem (DatTypClash    _) = true
  | issem (ConsArgNApp    _) = true
  | issem (ConsNArgApp    _) = true
  | issem RigidWhere         = true
  | issem (NonFlexWhere   _) = true
  | issem (IllFormedWhere _) = true
  | issem (MultiOcc       _) = false
  | issem (ValVarApp      _) = false
  | issem (ExcIsVar       _) = false
  | issem (ExcIsDat       _) = false
  | issem (ConIsVar       _) = false
  | issem (DatIsExc       _) = false
  | issem (TyVarBind      _) = false
  | issem (Warning        _) = false
  | issem (Parsing        _) = false
  | issem Inclusion          = false
  | issem AppNotApp          = false
  | issem DiffFunName        = false
  | issem DiffNbArgFun       = false
  | issem FreeTyVarTop       = false
  | issem AsPatVar           = false
  | issem FnRecExp           = false
  | issem RealInPat          = false
  | issem FreeIdent          = false
  | issem FreeOpen           = false

fun issyn x = not (issem x)

(* We use this function when minimising an arity error because these two labels will
 * stay in the minimal error. *)
fun getLabsEk (ArityClash ((lab1, _), (lab2, _))) = L.ord [L.fromInt lab1, L.fromInt lab2]
  | getLabsEk _ = L.empty

(*(* This does not seem to be used anymore.
 * Where and why was it used in the first place? *)
fun getLabelsErrorKind (ArityClash  ((l1, _), (l2, _))) = O.ord [l1, l2]
  | getLabelsErrorKind (TyConsClash ((l1, _), (l2, _))) = O.ord [l1, l2]
  | getLabelsErrorKind (NotGenClash ((l1, _), (l2, _))) = O.ord [l1, l2]
  | getLabelsErrorKind (TyFunClash  ((l1, _), (l2, _))) = O.ord [l1, l2]
  | getLabelsErrorKind (LabTyClash  (l1, l2, l3, l4))   = O.ord (map (fn (l, _) => l) (l1 @ l2 @ l3 @ l4))
  | getLabelsErrorKind (Unmatched   ((l, _), ls, lab))  = O.ord (l :: map (fn (l, _) => l) ls)
  | getLabelsErrorKind (UnbWhere    ((l, _), ls, lab))  = O.ord (l :: map (fn (l, _) => l) ls)
  | getLabelsErrorKind (MissConsSig ((l, _), ls))       = O.ord (l :: map (fn (l, _) => l) ls)
  | getLabelsErrorKind (MissConsStr ((l, _), ls))       = O.ord (l :: map (fn (l, _) => l) ls)
  | getLabelsErrorKind (DatTypClash (_, l1, l2))        = O.ord [l1, l2]
  | getLabelsErrorKind (ConsArgNApp (l1, l2))           = O.ord [l1, l2]
  | getLabelsErrorKind (ConsNArgApp (l1, l2))           = O.ord [l1, l2]
  | getLabelsErrorKind (Overload    (_, (l, _), ltns))  = O.ord (l :: (map (fn (l, _) => l) ltns))
  | getLabelsErrorKind Circularity    = O.empty
  | getLabelsErrorKind (MultiOcc   _) = O.empty
  | getLabelsErrorKind (ValVarApp  _) = O.empty
  | getLabelsErrorKind (ExcIsVar   _) = O.empty
  | getLabelsErrorKind (ExcIsDat   _) = O.empty
  | getLabelsErrorKind (ConIsVar   _) = O.empty
  | getLabelsErrorKind (DatIsExc   _) = O.empty
  | getLabelsErrorKind (TyVarBind  _) = O.empty
  | getLabelsErrorKind (Parsing    _) = O.empty
  | getLabelsErrorKind Inclusion      = O.empty
  | getLabelsErrorKind AppNotApp      = O.empty
  | getLabelsErrorKind DiffFunName    = O.empty
  | getLabelsErrorKind DiffNbArgFun   = O.empty
  | getLabelsErrorKind FreeTyVarTop   = O.empty
  | getLabelsErrorKind AsPatVar       = O.empty
  | getLabelsErrorKind FnRecExp       = O.empty
  | getLabelsErrorKind RealInPat      = O.empty
  | getLabelsErrorKind FreeIdent      = O.empty

(* This is not used anymore. *)
fun getNameErrorKind x = #2 (printErrKind x I.emAssoc)*)

end