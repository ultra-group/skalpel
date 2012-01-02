(* Copyright 2011 Heriot-Watt University
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
 *  o Authors:     John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        05 November 2011
 *  o File name:   RunSlicer.sml
 *  o Description: Parses the test answer JSON files
 *)

structure JsonParser : JsonParser =
struct

(* the sml/nj library structure *)
structure NJJP = JSONParser
structure EH = ErrorHandler
structure EK = ErrorKind
structure D  = Debug

(* the type which we use to represent a singular error *)
type oneerror = {labels       : int * int list,
		 assumptions  : LongId.keyOut list,
		 kind         : ErrorKind.kind,
		 slice        : string,
		 time         : LargeInt.int,
		 identifier   : int,
		 regions      : ExtReg.regs}

(* the type we use to represent the errors in the input program *)
type error = {errors       : oneerror list,
	      time         : {analysis     : LargeInt.int,
			      enumeration  : LargeInt.int,
			      minimisation : LargeInt.int,
			      slicing      : LargeInt.int,
			      html         : LargeInt.int},
	      tyvar        : int * Id.assocOut,
	      ident        : Id.assocOut,
	      constraint   : {syntactic : int,
			      top       : int,
			      total     : int},
	      labels       : int,
	      minimisation : bool,
	      solution     : int,
	      basis        : int,
	      timelimit    : LargeInt.int,
	      labelling    : string,
              final        : bool,
	      name         : string} option ref

fun parseTest testfile =
    let
	(* accessor methods for the different constructors of JSON.value *)
	fun getObject (JSON.OBJECT objectList) expectedString =
	    let
		fun getName (name, _)        = name
		fun getValue (_, value) = value
		val head = List.hd objectList
	    in
		if (getName head) = expectedString
		then (getValue head, (JSON.OBJECT (List.tl objectList)))
		else raise EH.DeadBranch ("Format error with JSON test file (expected object not found)")
	    end
	  | getObject _ _                    = raise EH.DeadBranch ("Format error with JSON test file (getObject got something other than an object)")
	fun getNull      (JSON.NULL)         = ()
	  | getNull      _                   = raise EH.DeadBranch ("Format error with JSON test file (getNull got something other than NULL)")
	fun getBool      (JSON.BOOL value)   = value
	  | getBool      _                   = raise EH.DeadBranch ("Format error with JSON test file (getBool got something other than a bool)")
	fun getInt       (JSON.INT value)    = (IntInf.toInt value:int)
	  | getInt      _                    = raise EH.DeadBranch ("Format error with JSON test file (getInt got something other than an int)")
	fun getIntArray  (JSON.ARRAY [])     = []
	  | getIntArray  (JSON.ARRAY (h::t)) = (getInt h::(getIntArray (JSON.ARRAY t)))
	  | getIntArray      _               = raise EH.DeadBranch ("Format error with JSON test file (getIntArray got something other than an array)")
	fun getIntInf    (JSON.INT value)    = value
	  | getIntInf      _                 = raise EH.DeadBranch ("Format error with JSON test file (getIntInf got something other than an int)")
	fun getFloat     (JSON.FLOAT value)  = value
	  | getFloat      _                  = raise EH.DeadBranch ("Format error with JSON test file (getFloat got something other than a float)")
	fun getString    (JSON.STRING value) = value
	  | getString      _                 = raise EH.DeadBranch ("Format error with JSON test file (getString got something other than a string)")
	fun getStrArray  (JSON.ARRAY [])     = []
	  | getStrArray  (JSON.ARRAY (h::t)) = (getString h::(getStrArray (JSON.ARRAY t)))
	  | getStrArray      _               = raise EH.DeadBranch ("Format error with JSON test file (getStrArray got something other than an array)")

	fun getNextObject (JSON.OBJECT([])) = raise EH.DeadBranch ("Format error with JSON test file (no next object)")
	  | getNextObject (JSON.OBJECT(((id,value)::t))) = (id, value)
	  | getNextObject _ = raise EH.DeadBranch ("Format error with JSON test file (getNextObject got something other than an object)")

	(* finds a value with a given identifier in an object *)
	fun findIdVal (JSON.OBJECT(object)) id =
	    let
		val (objectId, objectValue) = getNextObject (JSON.OBJECT(object))
	    in
		if id=objectId
		then objectValue
		else findIdVal (JSON.OBJECT(List.tl object)) id
	    end
	  | findIdVal _ _ = raise EH.DeadBranch ("Format error with JSON test file (findIdVal got something other than an object)")

	fun getTyvars    (JSON.ARRAY ([]))   = []
	  | getTyvars    (JSON.ARRAY (h::t)) = ((getInt(findIdVal h "id"), getString(findIdVal h "str"))::(getTyvars (JSON.ARRAY t)))
	  | getTyvars    _                   = raise EH.DeadBranch ("Format error with JSON test file (getTyvars got something other than an array)")

	fun parseRegions (JSON.ARRAY []) = []
	  | parseRegions (JSON.ARRAY(h::t)) =
	    let
		val (fileName, rest) = getObject h "fileName"
		val (fileRegions, _) = getObject rest "regionList"

		fun parseFileRegions (JSON.ARRAY []) = []
		  | parseFileRegions (JSON.ARRAY(h::t)) =
		    let
			val (nodeType, rest) = getObject h "nodeType"
			val (fromLine, rest) = getObject rest "fromLine"
			val (fromColumn, rest) = getObject rest "fromColumn"
			val (toLine, rest) = getObject rest "toLine"
			val (toColumn, rest) = getObject rest "toColumn"
			val (color, rest) = getObject rest "color"
			val (weight, rest) = getObject rest "weight"

			fun getColor "B"   = ExtReg.Blue
			  | getColor "R"    = ExtReg.Red
			  | getColor "P" = ExtReg.Purple
			  | getColor "G"  = ExtReg.Green
			  | getColor "O" = ExtReg.Orange
			  | getColor "Y" = ExtReg.Yellow
			  | getColor other = raise EH.DeadBranch ("Format error with JSON test file (getColor got \"" ^ other ^ "\", which is  something other than B, R, P, G, O or Y)")
		    in
			if getString(nodeType) = "node"
			then ExtReg.N({from=(getInt(fromLine),getInt(fromColumn)),to=(getInt(toLine), getInt(toColumn))}, getColor(getString color), getInt(weight), parseFileRegions(findIdVal rest "regionList"))::(parseFileRegions(JSON.ARRAY t))
			else if getString(nodeType) = "head"
			then ExtReg.H({from=(getInt(fromLine),getInt(fromColumn)),to=(getInt(toLine), getInt(toColumn))}, getColor(getString color), getInt(weight))::(parseFileRegions(JSON.ARRAY t))
			else ExtReg.L({from=(getInt(fromLine),getInt(fromColumn)),to=(getInt(toLine), getInt(toColumn))}, getColor(getString color), getInt(weight))::(parseFileRegions(JSON.ARRAY t))
		    end
		  | parseFileRegions _ = raise EH.DeadBranch ("Format error with JSON test file (parseFileRegions got something other than an array)")
	    in
		(getString(fileName), parseFileRegions fileRegions)::(parseRegions(JSON.ARRAY t))
	    end
	  | parseRegions _ = raise EH.DeadBranch ("Format error with JSON test file (parseRegions got something other than an array)")

	fun parseKind object =
	    let
		val (ek, rest) = getObject object "errorKindName"

		fun getTnerrList (JSON.ARRAY [])    = []
		  | getTnerrList (JSON.ARRAY(h::t)) =
		    let
			val (label, rest)  = getObject h "label"
			val (tyname, rest) = getObject rest "tyname"
		    in
			(getInt(label), getInt(tyname))::(getTnerrList (JSON.ARRAY(t)))
		    end
		  | getTnerrList _ = raise EH.DeadBranch ("Format error with JSON test file (getTnerrList got something other than an array)")

		fun getRecErr (JSON.ARRAY []) = []
		  | getRecErr (JSON.ARRAY (h::t)) =
		    let
			val (label, rest) = getObject h "label"
			val (str, rest) = getObject rest "string"
		    in
			(getInt label, getString str)::(getRecErr (JSON.ARRAY t))
		    end
		  | getRecErr _ = raise EH.DeadBranch ("Format error with JSON test file (getRecErr got something other than an array)")

		(* for the specerr list part of unmerr *)
		fun getUnmErr (JSON.ARRAY []) = []
		  | getUnmErr (JSON.ARRAY (h::t)) =
		    let
			val (label, rest) = getObject h "label"
			val (id, rest) = getObject rest "id"
		    in
			(getInt label, getInt id)::(getUnmErr (JSON.ARRAY t))
		    end
		  | getUnmErr _ = raise EH.DeadBranch ("Format error with JSON test file (getUnmErr got something other than an array)")
	    in
		case getString(ek) of
		    "ErrorKind.Circularity" => EK.Circularity
		  | "ErrorKind.Overload" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.Overload( (getInt(findIdVal errInfo "iderrLabel"), getInt(findIdVal errInfo "iderrId")),
				     (getInt(findIdVal errInfo "tnerrLabel"), getInt(findIdVal errInfo "tnerrTyname")),
				     getTnerrList (findIdVal errInfo "tnerrList") )
		    end
		  | "ErrorKind.OverloadCst" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.OverloadCst( (getInt(findIdVal errInfo "idserrLabel"), getInt(findIdVal errInfo "idserrId"), getString(findIdVal errInfo "idserrString")),
					(getInt(findIdVal errInfo "tnerrLabel"), getInt(findIdVal errInfo "tnerrTyname")),
					getTnerrList (findIdVal errInfo "tnerrList") )
		    end
		  | "ErrorKind.OverloadClash" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.OverloadClash( (getInt(findIdVal errInfo "idserrLabel1"), getInt(findIdVal errInfo "idserrId1"), getString(findIdVal errInfo "idserrString1")),
					 getTnerrList (findIdVal errInfo "tnerrList1"),
					 (getInt(findIdVal errInfo "idserrLabel2"), getInt(findIdVal errInfo "idserrId2"), getString(findIdVal errInfo "idserrString2")),
					 getTnerrList (findIdVal errInfo "tnerrList2") )
		    end
		  | "ErrorKind.OverloadIdCst" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.OverloadIdCst( (getInt(findIdVal errInfo "iderrLabel1"), getInt(findIdVal errInfo "iderrId1")),
					  getTnerrList (findIdVal errInfo "tnerrList1"),
					  (getInt(findIdVal errInfo "idserrLabel2"), getInt(findIdVal errInfo "idserrId2"), getString(findIdVal errInfo "idserrString2")),
					 getTnerrList (findIdVal errInfo "tnerrList2") )
		    end
		  | "ErrorKind.ArityClash" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.ArityClash( (getInt(findIdVal errInfo "arrerrLabel1"), getInt(findIdVal errInfo "arrerrId1")),
				       (getInt(findIdVal errInfo "arrerrLabel2"), getInt(findIdVal errInfo "arrerrId2")) )
		    end
		  | "ErrorKind.TyConsClash" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.TyConsClash( (getInt(findIdVal errInfo "tnerrLabel1"), getInt(findIdVal errInfo "tnerrTyname1")),
					(getInt(findIdVal errInfo "tnerrLabel2"), getInt(findIdVal errInfo "tnerrTyname2")) )
		    end
		  | "ErrorKind.NotGenClash" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.NotGenClash( (getInt(findIdVal errInfo "iderrLabel1"), getInt(findIdVal errInfo "iderrId1")),
					(getInt(findIdVal errInfo "tnerrLabel2"), getInt(findIdVal errInfo "tnerrTyname2")) )
		    end
		  | "ErrorKind.TooGenSig" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.TooGenSig( (getInt(findIdVal errInfo "iderrLabel1"), getInt(findIdVal errInfo "iderrId1")),
					(getInt(findIdVal errInfo "iderrLabel2"), getInt(findIdVal errInfo "iderrId2")),
					(getIntArray(findIdVal errInfo "labsList")) )
		    end
		  | "ErrorKind.TyFunClash" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.TyFunClash( (getInt(findIdVal errInfo "iderrLabel1"), getInt(findIdVal errInfo "iderrId1")),
				       (getInt(findIdVal errInfo "tnerrLabel2"), getInt(findIdVal errInfo "tnerrTyname2")) )
		    end
		  | "ErrorKind.LabTyClash" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.LabTyClash( getRecErr(findIdVal errInfo "laberr1"), getRecErr(findIdVal errInfo "laberr2"), getRecErr(findIdVal errInfo "laberr3"), getRecErr(findIdVal errInfo "laberr4") )
		    end
		  | "ErrorKind.Unmatched" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.Unmatched( (getInt(findIdVal errInfo "specerrLabel"), getInt(findIdVal errInfo "specerrId")),
				      (getUnmErr (findIdVal errInfo "unmerr")),
				      (getInt(findIdVal errInfo "unmerrLabel")) )
		    end
		  | "ErrorKind.UnbWhere" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.UnbWhere( (getInt(findIdVal errInfo "specerrLabel"), getInt(findIdVal errInfo "specerrId")),
				     (getUnmErr (findIdVal errInfo "unmerr")),
				     (getInt(findIdVal errInfo "unmerrLabel")) )
		    end
		  | "ErrorKind.MissConsSig" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.MissConsSig( (getInt(findIdVal errInfo "label"), getInt(findIdVal errInfo "id")),
					(getUnmErr (findIdVal errInfo "unmerr")) )
		    end
		  | "ErrorKind.MissConsStr" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.MissConsStr( (getInt(findIdVal errInfo "label"), getInt(findIdVal errInfo "id")),
					(getUnmErr (findIdVal errInfo "unmerr")) )
		    end
		  | "ErrorKind.DatTypClash" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.DatTypClash( getInt(findIdVal errInfo "id"), getInt(findIdVal errInfo "label1"), getInt(findIdVal errInfo "label2") )
		    end
		  | "ErrorKind.ConsArgNApp" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.ConsArgNApp( getInt(findIdVal errInfo "label1"), getInt(findIdVal errInfo "label2") )
		    end
		  | "ErrorKind.ConsNArgApp" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.ConsArgNApp( getInt(findIdVal errInfo "label1"), getInt(findIdVal errInfo "label2") )
		    end
		  | "ErrorKind.NonFlexWhere" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.NonFlexWhere( (getInt(findIdVal errInfo "iderr1Label"), getInt(findIdVal errInfo "iderr1Id")),
					 (getInt(findIdVal errInfo "iderr2Label"), getInt(findIdVal errInfo "iderr2Id")) )
		    end
		  | "ErrorKind.IllFormedWhere" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.IllFormedWhere( (getInt(findIdVal errInfo "iderr1Label"), getInt(findIdVal errInfo "iderr1Id")),
					   (getInt(findIdVal errInfo "iderr2Label"), getInt(findIdVal errInfo "iderr2Id")) )
		    end
		  | "ErrorKind.MultiOcc" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.MultiOcc (NONE)
			else EK.MultiOcc (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.ValVarApp" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.ValVarApp (NONE)
			else EK.ValVarApp (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.ExcIsVar" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.ExcIsVar (NONE)
			else EK.ExcIsVar (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.ExcIsDat" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.ExcIsDat (NONE)
			else EK.ExcIsDat (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.ConIsVar" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.ConIsVar (NONE)
			else EK.ConIsVar (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.DatIsExc" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.DatIsExc (NONE)
			else EK.DatIsExc (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.TyVarBind" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.TyVarBind (NONE)
			else EK.TyVarBind (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.Warning" => EK.Warning(getString(findIdVal rest "warningStr"))
		  | "ErrorKind.Parsing" => EK.Parsing(getString(findIdVal rest "parsingStr"))
		  | "ErrorKind.RigidWhere" => EK.RigidWhere
		  | "ErrorKind.Inclusion"  => EK.Inclusion
		  | "ErrorKind.AppNotApp"  => EK.AppNotApp
		  | "ErrorKind.DiffFunName"  => EK.DiffFunName
		  | "ErrorKind.DiffNbArgFun"  => EK.DiffNbArgFun
		  | "ErrorKind.FreeTyVarTop"  => EK.FreeTyVarTop
		  | "ErrorKind.AsPatVar"  => EK.AsPatVar
		  | "ErrorKind.FnRecExp"  => EK.FnRecExp
		  | "ErrorKind.RealInPat" => EK.RealInPat
		  | "ErrorKind.FreeIdent" => EK.FreeIdent
		  | "ErrorKind.FreeOpen"  => EK.FreeOpen
		  | _ => raise EH.DeadBranch ("Format error with JSON test file (unknown error kind)")
	    end

	fun getErrors (JSON.ARRAY []) = (D.printDebug 1 D.JSON "finished parsing errors object!"; [])
	  | getErrors (JSON.ARRAY(h::t)) = (D.printDebug 2 D.JSON "parsing errors object. Getting a new error...";
		{identifier  = getInt(findIdVal h "identifier"),
		 labels      = (getInt(findIdVal (findIdVal h "labels") "count"), getIntArray(findIdVal(findIdVal h "labels") "labelNumbers")),
                 assumptions = List.map (fn x => ([], x)) (getIntArray (findIdVal h "assumptions")), (* first part of tuple always empty list? *)
                 kind        = parseKind (findIdVal h "kind"),
	    (* jpirie: we want to do it this way but the SML/NJ JSON lexer is broken. To fix! *)
                 (* slice       = (Option.valOf(String.fromString(getString(findIdVal h "slice"))) *)
		 (* 		handle _ => raise EH.DeadBranch "Format error with JSON test file (slice malformed)"), *)
                 slice       = getString(findIdVal h "slice"),
                 time        = getIntInf(findIdVal h "time"),
		 regions     = parseRegions (findIdVal h "regions")}::(getErrors (JSON.ARRAY(t))))
	  | getErrors _ = raise EH.DeadBranch ("Format error with JSON test file (getErrors got something other than an array)")

	val test = NJJP.parseFile testfile handle _ => raise EH.DeadBranch ("Cannot parse JSON file: "^testfile^"\n")
	val _ = D.printDebug 1 D.JSON ("checking top-level objects of json file...")
	val _ = D.printDebug 2 D.JSON ("getting 'errors' object...")
	val (errorList, test) = getObject test "errors"
	val _ = D.printDebug 2 D.JSON ("getting 'time' object...")
	val (timeObj, test) = getObject test "time"
	val _ = D.printDebug 2 D.JSON ("getting 'tyvar' object...")
	val (tyvarObj, test) = getObject test "tyvar"
	val _ = D.printDebug 2 D.JSON ("getting 'ident' object...")
	val (ident, test) = getObject test "ident"
	val _ = D.printDebug 2 D.JSON ("getting 'constraint' object...")
	val (constraintObj, test) = getObject test "constraint"
	val _ = D.printDebug 2 D.JSON ("getting 'labels' object...")
	val (labels, test) = getObject test "labels"
	val _ = D.printDebug 2 D.JSON ("getting 'minimisation' object...")
	val (minimisation, test) = getObject test "minimisation"
	val _ = D.printDebug 2 D.JSON ("getting 'solution' object...")
	val (solution, test) = getObject test "solution"
	val _ = D.printDebug 2 D.JSON ("getting 'basis' object...")
	val (basis, test) = getObject test "basis"
	val _ = D.printDebug 2 D.JSON ("getting 'timelimit' object...")
	val (timelimit, test) = getObject test "timelimit"
	val _ = D.printDebug 2 D.JSON ("getting 'labelling' object...")
	val (labelling, test) = getObject test "labelling"
	val _ = D.printDebug 2 D.JSON ("getting final object...")
	val (final, test) = getObject test "final"
	val _ = D.printDebug 2 D.JSON ("getting 'name' object...")
	val (name, test) = getObject test "name"
	val _ = D.printDebug 1 D.JSON ("top level json objects correct!")

    in
	ref (SOME {
	     errors = (getErrors errorList),
	     time        = {analysis = getIntInf((findIdVal timeObj "analysis")),
			    enumeration = getIntInf((findIdVal timeObj "enumeration")),
			    minimisation = getIntInf((findIdVal timeObj "minimisation")),
			    slicing = getIntInf((findIdVal timeObj "slicing")),
			    html = getIntInf((findIdVal timeObj "html"))},
	     tyvar       = (getInt(findIdVal tyvarObj "tyvar"),
			    getTyvars(findIdVal tyvarObj "assoc")),
	     ident       = getTyvars(ident),
	     constraint = {total = getInt((findIdVal constraintObj "total")),
			   top = getInt((findIdVal constraintObj "top")),
			   syntactic = getInt((findIdVal constraintObj "syntactic"))},
	     labels = getInt(labels),
	     minimisation = getBool(minimisation),
	     solution = getInt(solution),
	     basis = getInt(basis),
	     timelimit = getIntInf(timelimit),
	     labelling = getString(labelling),
	     final = getBool(final),
	     name = getString(name)
	    })
    end
end