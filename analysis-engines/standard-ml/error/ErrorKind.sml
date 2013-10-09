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
 *  o Date:        24 May 2010
 *  o File name:   ErrorKind.sml
 *)

(** Contains the definition of the different kinds of errors handled by our slicer (opaquely constrained by refstruct{ERRORKIND}). *)
structure ErrorKind :> ERRORKIND = struct

structure T  = Ty
structure I  = Id
structure L  = Label
structure EH = ErrorHandler

(** External label: integer instead of Label.label. *)
type label    = int
(** External id: integer instead of Id.id. *)
type id       = int
(** External tyname: integer instead of Ty.tyname. *)
type typename   = int
(** A field name in a record. *)
type laberr   = (label * string) list
(** A record error: the first laberr are for the clashing fields in two clashing records, the last two ones are for the common fields. *)
type recerr   = laberr * laberr * laberr * laberr

(** An identifier in a structure or signature or where clause. *)
type specerr  = label * id
(** The arity of a sequence. *)
type arrerr   = label * int
(** An identifier. *)
type iderr    = label * id
(** A constant. *)
type idserr   = label * id * string
(** A type constructor. *)
type tnerr    = label * typename
(** An unmatched error for a identifier not declared in a structure/signature. *)
type unmerr   = specerr * specerr list * label
(** A snytactic error? *)
type synerr   = (int list * int) option

(* in the Overload errors, need to add the labels associated with the overloading classes. *)


(*************************************************************
 * WARNING: Changing the names of these constructors will break tests in the test database, so
 *          when making any changes to the names of the constructors, changes MUST be made
 *          there also. Furthermore, changes need to be made in JsonParser.sml, which also
 *          has a list of these type constructors
 ************************************************************)

(** A datatype containing all of all the different kinds of error that Skalpel can represent.
 * Currently includes the following list:
 * \arg Circularity.
 * \arg Overload.
 * \arg OverloadCst.
 * \arg OverloadClash.
 * \arg OverloadIdCst.
 * \arg ArityClash.
 * \arg TyConsClash.
 * \arg EqTypeRequired.
 * \arg NotGenClash.
 * \arg TooGenSig.
 * \arg TyFunClash.
 * \arg LabTyClash.
 * \arg Unmatched.
 * \arg UnbWhere.
 * \arg DatTypClash.
 * \arg ConsArgNApp.
 * \arg ConsNArgApp.
 * \arg MissConsSig.
 * \arg MissConsStr.
 * \arg MultiOcc.
 * \arg ValVarApp.
 * \arg ExcIsVar.
 * \arg ExcIsDat.
 * \arg ConIsVar.
 * \arg DatIsExc.
 * \arg TypeVarBind.
 * \arg Warning.
 * \arg Parsing.
 * \arg NonFlexWhere.
 * \arg IllFormedWhere.
 * \arg RigidWhere.
 * \arg Inclusion.
 * \arg AppNotApp.
 * \arg DiffFunName.
 * \arg DiffNbArgFun.
 * \arg FreeTypeVarTop.
 * \arg AsPatVar.
 * \arg FnRecExp.
 * \arg RealInPat.
 * \arg FreeIdent.
 * \arg FreeOpen.
 *)
datatype kind = Circularity
	      | Overload       of iderr  * tnerr * tnerr list (* value iderr overloaded to tnerrlist used on tnerr     *)
	      | OverloadCst    of idserr * tnerr * tnerr list (* constant idserr overloaded to tnerrlist used on tnerr *)
	      | OverloadClash  of idserr * tnerr list * idserr * tnerr list (* two constaint constrained to be equal but overloaded to different types *)
	      | OverloadIdCst  of iderr  * tnerr list * idserr * tnerr list (* value iderr overloaded to tnerrlist used on idserr                      *)
              | ArityClash     of arrerr * arrerr
              | TyConsClash    of tnerr  * tnerr
	      | EqTypeRequired of label  * label
	      | NotGenClash    of iderr  * tnerr
	      | TooGenSig      of iderr  * iderr * label list (* the identifier iderr(1st) is monomorphic because of the label list but its specification contains the variable iderr(2nd) *)
	      | TyFunClash     of iderr  * tnerr
	      | LabTyClash     of recerr
	      | Unmatched      of unmerr
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
	      | TypeVarBind    of synerr
	      | Warning        of string
	      | Parsing        of string
	      | NonFlexWhere   of iderr * iderr
	      | IllFormedWhere of iderr * iderr
	      | RigidWhere
	      | Inclusion
	      | AppNotApp
	      | DiffFunName
	      | DiffNbArgFun
	      | FreeTypeVarTop
	      | AsPatVar
	      | FnRecExp
	      | RealInPat
	      | FreeIdent
	      | FreeOpen

(** Converts a #label value to a string. *)
fun printLab lab = Int.toString lab

(** Pritns out a list. *)
fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Tests whether the string given as an argument is an arithmetic operator, either +,-,* or /. *)
fun isArithOp x =
    (x="+" orelse x="-" orelse x="*" orelse x="/")

(* PRINTING FOR LISP *)

(** Gets the string of an identifier. *)
fun getSt n asc = I.printId' (I.fromInt n) asc

(** Attems to remove a field name from a record. *)
fun removeFieldName _ [] = (false, [])
  | removeFieldName (st : string) ((x as (_, st')) :: xs) =
    if st = st'
    then (true, xs)
    else (fn (b, xs') => (b, x :: xs')) (removeFieldName st xs)

(** Compares the field names of a record, returns true if the fields match, false otherwise. *)
fun compareFieldNames [] [] = true
  | compareFieldNames [] _  = false
  | compareFieldNames ((_, st) :: xs) ys =
    let
	val (b, zs) = removeFieldName st ys
    in b andalso compareFieldNames xs zs
    end

(** Used to print the fields of a record *)
fun printFields xs =
    "{" ^
    #1 (foldr
	    (fn ((_, lc), (y, z)) => (lc ^ z ^ y, ","))
	    ("", "")
	    xs) ^
    "}"

(** Prits an error kind. *)
fun printErrKind Circularity _ = ("CIR", "Circularity")
  | printErrKind (Overload ((lid, id), (l, tn), ltns)) asc =
    ("OVE",
     "Variable "
     ^ getSt id asc
     ^ " overloaded to a list of types not including "
     ^ T.printTypename' (T.typenameFromInt tn))
  | printErrKind (OverloadCst ((lid, id, str), (l, tn), ltns)) asc =
    ("OVC",
     "Constant "
     ^ str
     ^ " overloaded to the overloading class "
     ^ getSt id asc
     ^  " not including "
     ^ T.printTypename' (T.typenameFromInt tn))
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
    let
	val value = getSt id1 asc
	val ty = getSt id2 asc
    in
	(* if this is the case, a more accurate slice will be reported later, but give the user something fast *)
	if isArithOp value andalso (ty = "Int" orelse ty = "Real")
	then
	    ("OIC", "Value " ^ value ^ " used on two different types.")
	else
	    ("OIC",
	     "Value " ^ value ^ " overloaded to a list of types not including any type of " ^
	     str ^ " itself overloaded to the overloading class " ^ ty)
    end
  | printErrKind (ArityClash ((l1, n1), (l2, n2))) _ =
    ("ARI",
     "Arity clash between "
     ^ Int.toString n1
     ^ " and "
     ^ Int.toString n2)
  | printErrKind (TyConsClash ((l1, tn1), (l2, tn2))) asc =
    ("TYP",
     "Type constructor clash between "
     ^ T.printTypename' (T.typenameFromInt tn1)
     ^ " and "
     ^ T.printTypename' (T.typenameFromInt tn2))
  | printErrKind (EqTypeRequired (l1)) asc =
    ("ETR",
     "Equality type required")
  | printErrKind (NotGenClash ((l1, tv), (l2, tn))) _ =
    ("GEN",
     "Structure's signature too general at type "
     ^ T.printTypename' (T.typenameFromInt tn))
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
  | printErrKind (TypeVarBind  eo) _ = ("UNG", "Ungeneralisable bound type variable")
  | printErrKind Inclusion       _ = ("INC", "Unbound type variable in type or datatype declaration")
  | printErrKind AppNotApp       _ = ("APP", "Applied and not applied value")
  | printErrKind (ConsArgNApp _) _ = ("CAN", "Non applied constructor that is defined to take an argument")
  | printErrKind (ConsNArgApp _) _ = ("CNA", "Applied constructor that is defined to take no argument")
  | printErrKind DiffFunName     _ = ("FUN", "Different function name")
  | printErrKind DiffNbArgFun    _ = ("ARG", "Different number of arguments")
  | printErrKind FreeTypeVarTop    _ = ("FRE", "Free type variable at top-level")
  | printErrKind AsPatVar        _ = ("LAS", "Left of 'as' must be a variable")
  | printErrKind FnRecExp        _ = ("FNE", "Expressions within rec value bindings must be functions")
  | printErrKind RealInPat       _ = ("REA", "Reals cannot occur within patterns")
  | printErrKind FreeIdent       _ = ("IDE", "Free identifier")
  | printErrKind FreeOpen        _ = ("IDE", "Free opened or replicated identifier (hidding some context)")

fun printSmlAsmpStr []        = ""
  | printSmlAsmpStr [x]       = Int.toString x
  | printSmlAsmpStr (x :: xs) = Int.toString x ^ "," ^ printSmlAsmpStr xs

fun printSmlAsmpOp NONE           = "NONE"
  | printSmlAsmpOp (SOME (nl, n)) = "SOME ([" ^ printSmlAsmpStr nl ^ "]," ^ Int.toString n ^ ")"

fun printJsonAsmpOp NONE           = "\"asmpOp\": \"NONE\"}"
  | printJsonAsmpOp (SOME (nl, n)) = "\"asmpOp\": {\"synerrList\": [" ^ printSmlAsmpStr nl ^ "], \"synerrInt\": " ^ Int.toString n ^ "}}"

(** Printing a record clash (SML format). *)
fun printSmlLabErr []              = ""
  | printSmlLabErr [(l, st)]       = "(" ^ printLab l ^ ",\"" ^ st ^ "\")"
  | printSmlLabErr ((l, st) :: xs) = "(" ^ printLab l ^ ",\"" ^ st ^ "\")," ^ printSmlLabErr xs

(** Printing a record clash (JSON format). *)
fun printJsonLabErr []              = ""
  | printJsonLabErr [(l, st)]       = "{ \"label\": " ^ printLab l ^ ", \"string\": \"" ^ st ^ "\" }"
  | printJsonLabErr ((l, st) :: xs) = "{ \"label\": " ^ printLab l ^ ", \"string\": \"" ^ st ^ "\" }, " ^ printJsonLabErr xs

(** Printing unmatched errors (SML format). *)
fun printSmlUnm []             = ""
  | printSmlUnm [(l, n)]       = "(" ^ printLab l ^ "," ^ Int.toString n ^ ")"
  | printSmlUnm ((l, n) :: xs) = "(" ^ printLab l ^ "," ^ Int.toString n ^ ")," ^ printSmlUnm xs

(** Printing unmatched errors (JSON format). *)
fun printJsonUnm []             = ""
  | printJsonUnm [(l, n)]       = "{ \"label\": " ^ printLab l ^ ", \"id\": " ^ Int.toString n ^ " }"
  | printJsonUnm ((l, n) :: xs) = "{ \"label\": " ^ printLab l ^ ", \"id\": " ^ Int.toString n ^ " }," ^ printJsonUnm xs


(** Prints typenames in SML format. *)
fun printSmlLabTypenames []              = ""
  | printSmlLabTypenames [(l, tn)]       = "(" ^ printLab l ^ "," ^ T.printsmltn (T.typenameFromInt tn) ^ ")"
  | printSmlLabTypenames ((l, tn) :: xs) = "(" ^ printLab l ^ "," ^ T.printsmltn (T.typenameFromInt tn) ^ ")," ^ printSmlLabTypenames xs

(** Prints typenames in JSON format. *)
fun printJsonLabTypenames []             = ""
  | printJsonLabTypenames [(l, tn)]       = "{ \"label\": " ^ printLab l ^ ", \"Typename\": " ^ T.printsmltn (T.typenameFromInt tn) ^ "}"
  | printJsonLabTypenames ((l, tn) :: xs) = "{ \"label\": " ^ printLab l ^ ", \"Typename\": " ^ T.printsmltn (T.typenameFromInt tn) ^ "}, " ^ printJsonLabTypenames xs

(** Escapes a string. *)
val transfun = fn #"\n" => ""
		| #"\\" => "\\\\"
		| #"\"" => "\\\""
		| x     => Char.toString x

(** Prints an error kind in SML format. *)
fun printSmlErrKind Circularity = "ErrorKind.Circularity"
  | printSmlErrKind (Overload ((id, lid), (l, tn), ltns)) =
    "ErrorKind.Overload("
    ^ "(" ^ printLab id ^ "," ^ Int.toString lid ^ "),"
    ^ "(" ^ printLab l  ^ "," ^ T.printsmltn (T.typenameFromInt tn)  ^ "),"
    ^ "[" ^ printSmlLabTypenames ltns ^ "])"
  | printSmlErrKind (OverloadCst ((id, lid, str), (l, tn), ltns)) =
    "ErrorKind.OverloadCst("
    ^ "(" ^ printLab id ^ "," ^ Int.toString lid ^ ",\"" ^ str ^ "\"" ^ "),"
    ^ "(" ^ printLab l  ^ "," ^ T.printsmltn (T.typenameFromInt tn)  ^ "),"
    ^ "[" ^ printSmlLabTypenames ltns ^ "])"
  | printSmlErrKind (OverloadClash ((id1, lid1, str1), ltns1, (id2, lid2, str2), ltns2)) =
    "ErrorKind.OverloadClash("
    ^ "(" ^ printLab id1 ^ "," ^ Int.toString lid1 ^ ",\"" ^  String.translate transfun str1 ^ "\"" ^ "),"
    ^ "[" ^ printSmlLabTypenames ltns1 ^ "],"
    ^ "(" ^ printLab id2 ^ "," ^ Int.toString lid2 ^ ",\"" ^  String.translate transfun str2 ^ "\"" ^ "),"
    ^ "[" ^ printSmlLabTypenames ltns2 ^ "])"
  | printSmlErrKind (OverloadIdCst ((id1, lid1), ltns1, (id2, lid2, str2), ltns2)) =
    "ErrorKind.OverloadIdCst("
    ^ "(" ^ printLab id1 ^ "," ^ Int.toString lid1 ^ "),"
    ^ "[" ^ printSmlLabTypenames ltns1 ^ "],"
    ^ "(" ^ printLab id2 ^ "," ^ Int.toString lid2 ^ ",\"" ^ String.translate transfun str2 ^ "\"" ^ "),"
    ^ "[" ^ printSmlLabTypenames ltns2 ^ "])"
  | printSmlErrKind (ArityClash ((l1, n1), (l2, n2))) =
    "ErrorKind.ArityClash("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString n1 ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ Int.toString n2 ^ "))"
  | printSmlErrKind (TyConsClash ((l1, tn1), (l2, tn2))) =
    "ErrorKind.TyConsClash("
    ^ "(" ^ printLab l1 ^ "," ^ T.printsmltn (T.typenameFromInt tn1) ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ T.printsmltn (T.typenameFromInt tn2) ^ "))"
  | printSmlErrKind (EqTypeRequired (l1,l2)) =
    "ErrorKind.EqTypeRequired("
    ^ "(" ^ printLab l1 ^ "," ^ printLab l2 ^ "))"
  | printSmlErrKind (NotGenClash ((l1, tv), (l2, tn))) =
    "ErrorKind.NotGenClash("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString tv ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ T.printsmltn (T.typenameFromInt tn) ^ "))"
  | printSmlErrKind (TooGenSig ((l1, id), (l2, tv), labs)) =
    "ErrorKind.TooGenSig("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString id ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ Int.toString tv ^ "),"
    ^ printlistgen labs Int.toString ^ ")"
  | printSmlErrKind (TyFunClash ((l1, tv), (l2, tn))) =
    "ErrorKind.TyFunClash("
    ^ "(" ^ printLab l1 ^ "," ^ Int.toString tv ^ "),"
    ^ "(" ^ printLab l2 ^ "," ^ T.printsmltn (T.typenameFromInt tn) ^ "))"
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
  | printSmlErrKind (TypeVarBind  eo) = "ErrorKind.TypeVarBind("  ^ printSmlAsmpOp eo ^ ")"
  | printSmlErrKind (Warning    st) = "ErrorKind.Warning(\""  ^ st ^ "\")"
  | printSmlErrKind (Parsing    st) = "ErrorKind.Parsing(\""  ^ st ^ "\")"
  | printSmlErrKind RigidWhere      = "ErrorKind.RigidWhere"
  | printSmlErrKind Inclusion       = "ErrorKind.Inclusion"
  | printSmlErrKind AppNotApp       = "ErrorKind.AppNotApp"
  | printSmlErrKind DiffFunName     = "ErrorKind.DiffFunName"
  | printSmlErrKind DiffNbArgFun    = "ErrorKind.DiffNbArgFun"
  | printSmlErrKind FreeTypeVarTop    = "ErrorKind.FreeTypeVarTop"
  | printSmlErrKind AsPatVar        = "ErrorKind.AsPatVar"
  | printSmlErrKind FnRecExp        = "ErrorKind.FnRecExp"
  | printSmlErrKind RealInPat       = "ErrorKind.RealInPat"
  | printSmlErrKind FreeIdent       = "ErrorKind.FreeIdent"
  | printSmlErrKind FreeOpen        = "ErrorKind.FreeOpen"

(** Prints an error kind in JSON format. *)
fun printJsonErrKind Circularity = "{\"errorKindName\": \"ErrorKind.Circularity\"}"
  | printJsonErrKind (Overload ((id, lid), (l, tn), ltns)) =
    "{\"errorKindName\": \"ErrorKind.Overload\", \"errorKindInfo\": {"
    ^ "\"iderrLabel\": " ^ printLab id ^ ", \"iderrId\": " ^ Int.toString lid
    ^ ", \"tnerrLabel\": " ^ printLab l  ^ ", \"tnerrTypename\": " ^ T.printsmltn (T.typenameFromInt tn)
    ^ ", \"tnerrList\": [" ^ printJsonLabTypenames ltns ^ "]}}"
  | printJsonErrKind (OverloadCst ((id, lid, str), (l, tn), ltns)) =
    "{\"errorKindName\": \"ErrorKind.OverloadCst\", \"errorKindInfo\": {"
    ^ "\"idserrLabel\": " ^ printLab id ^ ", \"idserrId\": " ^ Int.toString lid ^ ", \"idserrString\": \""^ str ^"\""
    ^ ", \"tnerrLabel\": " ^ printLab l  ^ ", \"tnerrTypename\": " ^ T.printsmltn (T.typenameFromInt tn)
    ^ ", \"tnerrList\": [" ^ printJsonLabTypenames ltns ^ "]}}"
  | printJsonErrKind (OverloadClash ((id1, lid1, str1), ltns1, (id2, lid2, str2), ltns2)) =
    "{\"errorKindName\": \"ErrorKind.OverloadClash\", \"errorKindInfo\": {"
    ^ "\"idserrLabel1\": " ^ printLab id1 ^ ", \"idserrId1\": " ^ Int.toString lid1 ^ ", \"idserrString1\": \""^ String.translate transfun str1 ^"\""
    ^ ", \"tnerrList1\": [" ^ printJsonLabTypenames ltns1 ^ "]"
    ^ ", \"idserrLabel2\": " ^ printLab id2 ^ ", \"idserrId2\": " ^ Int.toString lid2 ^ ", \"idserrString2\": \""^ String.translate transfun str2 ^"\""
    ^ ", \"tnerrList2\": [" ^ printJsonLabTypenames ltns2 ^ "]}}"
  | printJsonErrKind (OverloadIdCst ((id1, lid1), ltns1, (id2, lid2, str2), ltns2)) =
    "{\"errorKindName\": \"ErrorKind.OverloadIdCst\", \"errorKindInfo\": {"
    ^ "\"iderrLabel1\": " ^ printLab id1 ^ ", \"iderrId1\": " ^ Int.toString lid1
    ^ ", \"tnerrList1\": [" ^ printJsonLabTypenames ltns1 ^ "]"
    ^ ", \"idserrLabel2\": " ^ printLab id2 ^ ", \"idserrId2\": " ^ Int.toString lid2 ^ ", \"idserrString2\": \""^ String.translate transfun str2 ^"\""
    ^ ", \"tnerrList2\": [" ^ printJsonLabTypenames ltns2 ^ "]}}"
  | printJsonErrKind (ArityClash ((l1, n1), (l2, n2))) =
    "{\"errorKindName\": \"ErrorKind.ArityClash\", \"errorKindInfo\": {"
    ^ "\"arrerrLabel1\": " ^ printLab l1 ^ ", \"arrerrId1\": " ^ Int.toString n1
    ^ ", \"arrerrLabel2\": " ^ printLab l2 ^ ", \"arrerrId2\": " ^ Int.toString n2 ^"}}"
  | printJsonErrKind (TyConsClash ((l1, tn1), (l2, tn2))) =
    "{\"errorKindName\": \"ErrorKind.TyConsClash\", \"errorKindInfo\": {"
    ^ "\"tnerrLabel1\": " ^ printLab l1 ^ ", \"tnerrTypename1\": " ^ T.printsmltn (T.typenameFromInt tn1)
    ^ ", \"tnerrLabel2\": " ^ printLab l2 ^ ", \"tnerrTypename2\": " ^ T.printsmltn (T.typenameFromInt tn2) ^"}}"
  | printJsonErrKind (EqTypeRequired (l1, l2)) =
    "{\"errorKindName\": \"ErrorKind.EqTypeRequired\", \"errorKindInfo\": {"
    ^ "\"tnerrLabel1\": " ^ printLab l1 ^", \"tnerrLabel2\": " ^ printLab l2 ^"}}"
  | printJsonErrKind (NotGenClash ((l1, tv), (l2, tn))) =
    "{\"errorKindName\": \"ErrorKind.NotGenClash\", \"errorKindInfo\": {"
    ^ "\"iderrLabel1\": " ^ printLab l1 ^ ", \"iderrId1\": " ^ Int.toString tv
    ^ ", \"tnerrLabel2\": " ^ printLab l2 ^ ", \"tnerrTypename2\": " ^ T.printsmltn (T.typenameFromInt tn) ^"}}"
  | printJsonErrKind (TooGenSig ((l1, id), (l2, tv), labs)) =
    "{\"errorKindName\": \"ErrorKind.TooGenSig\", \"errorKindInfo\": {"
    ^ "\"iderrLabel1\": " ^ printLab l1 ^ ", \"iderrId1\": " ^ Int.toString id
    ^ ", \"iderrLabel2\": " ^ printLab l2 ^ ", \"iderrId2\": " ^ Int.toString tv
    ^ ", \"labsList\": " ^ printlistgen labs Int.toString ^ "}}"
  | printJsonErrKind (TyFunClash ((l1, tv), (l2, tn))) =
    "{\"errorKindName\": \"ErrorKind.TyFunClash\", \"errorKindInfo\": {"
    ^ "\"iderrLabel1\": " ^ printLab l1 ^ ", \"iderrId1\": " ^ Int.toString tv
    ^ ", \"tnerrLabel2\": " ^ printLab l2 ^ ", \"tnerrTypename2\": " ^ T.printsmltn (T.typenameFromInt tn) ^"}}"
  | printJsonErrKind (LabTyClash (ll1, ll2, ll3, ll4)) =
    "{\"errorKindName\": \"ErrorKind.LabTyClash\", \"errorKindInfo\": {"
    ^ "\"laberr1\": [" ^ printJsonLabErr ll1 ^ "], \"laberr2\": [" ^ printJsonLabErr ll2 ^ "], "
    ^ "\"laberr3\": [" ^ printJsonLabErr ll3 ^ "]" ^ ", \"laberr4\": [" ^ printJsonLabErr ll4 ^ "]" ^ "}}"
  | printJsonErrKind (Unmatched ((l, n), ls, lab)) =
    "{\"errorKindName\": \"ErrorKind.Unmatched\", \"errorKindInfo\": {"
    ^ "\"specerrLabel\": " ^ printLab l ^ ", \"specerrId\": " ^ Int.toString n
    ^ ", \"unmerr\": " ^ "[" ^ printJsonUnm ls ^ "]"
    ^ ", \"unmerrLabel\": " ^ printLab lab ^ "}}"
  | printJsonErrKind (UnbWhere ((l, n), ls, lab)) =
    "{\"errorKindName\": \"ErrorKind.UnbWhere\", \"errorKindInfo\": {"
    ^ "\"specerrLabel\": " ^ printLab l ^ ", \"specerrId\": " ^ Int.toString n
    ^ ", \"unmerr\": " ^ "[" ^ printJsonUnm ls ^ "]"
    ^ ", \"unmerrLabel\": " ^ printLab lab ^ "}}"
  | printJsonErrKind (MissConsSig ((l, n), ls)) =
    "{\"errorKindName\": \"ErrorKind.MissConsSig\", \"errorKindInfo\": {"
    ^ "\"label\": " ^ printLab l ^ ", \"id\": " ^ Int.toString n
    ^ ", \"labelidList\": [" ^ printJsonUnm ls ^ "]}}"
  | printJsonErrKind (MissConsStr ((l, n), ls)) =
    "{\"errorKindName\": \"ErrorKind.MissConsStr\", \"errorKindInfo\": {"
    ^ "\"label\": " ^ printLab l ^ ", \"id\": " ^ Int.toString n
    ^ ", \"labelidList\": [" ^ printJsonUnm ls ^ "]}}"
  | printJsonErrKind (DatTypClash (n, l1, l2)) =
    "{\"errorKindName\": \"ErrorKind.DatTypClash\", \"errorKindInfo\": {"
    ^ "\"id\": " ^ Int.toString n ^ ", \"label1\": " ^ printLab l1 ^ ", \"label2\": " ^ printLab l2 ^ "}}"
  | printJsonErrKind (ConsArgNApp (l1, l2)) =
    "{\"errorKindName\": \"ErrorKind.ConsArgNApp\", \"errorKindInfo\": {"
    ^ "\"label1\": " ^ printLab l1 ^ ", \"label2\": " ^ printLab l2 ^ "}}"
  | printJsonErrKind (ConsNArgApp (l1, l2)) =
    "{\"errorKindName\": \"ErrorKind.ConsNArgApp\", \"errorKindInfo\": {"
    ^ "\"label1\": " ^ printLab l1 ^ ", \"label2\": " ^ printLab l2 ^ "}}"
  | printJsonErrKind (NonFlexWhere ((lab1, id1), (lab2, id2))) =
    "{\"errorKindName\": \"ErrorKind.NonFlexWhere\", \"errorKindInfo\": {"
    ^ "\"iderr1Label\": " ^ printLab lab1 ^ ", \"iderr1Id\": " ^ Int.toString id1
    ^ ", \"iderr2Label\": " ^ printLab lab2 ^ ", \"iderr2Id\": " ^ Int.toString id2 ^ "}}"
  | printJsonErrKind (IllFormedWhere ((lab1, id1), (lab2, id2))) =
    "{\"errorKindName\": \"ErrorKind.IllFormedWhere\", \"errorKindInfo\": {"
    ^ "\"iderr1Label\": " ^ printLab lab1 ^ ", \"iderr1Id\": " ^ Int.toString id1
    ^ ", \"iderr2Label\": " ^ printLab lab2 ^ ", \"iderr2Id\": " ^ Int.toString id2 ^ "}}"
  | printJsonErrKind (MultiOcc   eo) = "{\"errorKindName\": \"ErrorKind.MultiOcc\", "   ^ printJsonAsmpOp eo
  | printJsonErrKind (ValVarApp  eo) = "{\"errorKindName\": \"ErrorKind.ValVarApp\", "  ^ printJsonAsmpOp eo
  | printJsonErrKind (ExcIsVar   eo) = "{\"errorKindName\": \"ErrorKind.ExcIsVar\", "   ^ printJsonAsmpOp eo
  | printJsonErrKind (ExcIsDat   eo) = "{\"errorKindName\": \"ErrorKind.ExcIsDat\", "   ^ printJsonAsmpOp eo
  | printJsonErrKind (ConIsVar   eo) = "{\"errorKindName\": \"ErrorKind.ConIsVar\", "   ^ printJsonAsmpOp eo
  | printJsonErrKind (DatIsExc   eo) = "{\"errorKindName\": \"ErrorKind.DatIsExc\", "   ^ printJsonAsmpOp eo
  | printJsonErrKind (TypeVarBind  eo) = "{\"errorKindName\": \"ErrorKind.TypeVarBind\", "  ^ printJsonAsmpOp eo
  | printJsonErrKind (Warning    st) = "{\"errorKindName\": \"ErrorKind.Warning\", \"warningStr\": \"" ^ st ^ "\"}"
  | printJsonErrKind (Parsing    st) = "{\"errorKindName\": \"ErrorKind.Parsing\", \"parsingStr\": \"" ^ st ^ "\"}"
  | printJsonErrKind RigidWhere      = "{\"errorKindName\": \"ErrorKind.RigidWhere\"}"
  | printJsonErrKind Inclusion       = "{\"errorKindName\": \"ErrorKind.Inclusion\"}"
  | printJsonErrKind AppNotApp       = "{\"errorKindName\": \"ErrorKind.AppNotApp\"}"
  | printJsonErrKind DiffFunName     = "{\"errorKindName\": \"ErrorKind.DiffFunName\"}"
  | printJsonErrKind DiffNbArgFun    = "{\"errorKindName\": \"ErrorKind.DiffNbArgFun\"}"
  | printJsonErrKind FreeTypeVarTop    = "{\"errorKindName\": \"ErrorKind.FreeTypeVarTop\"}"
  | printJsonErrKind AsPatVar        = "{\"errorKindName\": \"ErrorKind.AsPatVar\"}"
  | printJsonErrKind FnRecExp        = "{\"errorKindName\": \"ErrorKind.FnRecExp\"}"
  | printJsonErrKind RealInPat       = "{\"errorKindName\": \"ErrorKind.RealInPat\"}"
  | printJsonErrKind FreeIdent       = "{\"errorKindName\": \"ErrorKind.FreeIdent\"}"
  | printJsonErrKind FreeOpen        = "{\"errorKindName\": \"ErrorKind.FreeOpen\"}"

(** Returns whether the error kind is semantic/syntactic - true for semantic, false for syntactic. *)
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
  | issem (EqTypeRequired _) = true
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
  | issem (TypeVarBind      _) = false
  | issem (Warning        _) = false
  | issem (Parsing        _) = false
  | issem Inclusion          = false
  | issem AppNotApp          = false
  | issem DiffFunName        = false
  | issem DiffNbArgFun       = false
  | issem FreeTypeVarTop       = false
  | issem AsPatVar           = false
  | issem FnRecExp           = false
  | issem RealInPat          = false
  | issem FreeIdent          = false
  | issem FreeOpen           = false

(** Returns whether the error kind is syntactic - true for syntactic, false for syntactic.
 * Uses NOT(#issem (argument)) to compute return value. *)
fun issyn x = not (issem x)

(** We use this function when minimising an arity error because these two labels will stay in the minimal error. *)
fun getLabsEk (ArityClash ((lab1, _), (lab2, _))) = L.ord [L.fromInt lab1, L.fromInt lab2]
  | getLabsEk _ = L.empty ()

end
