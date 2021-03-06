(* Copyright 2011 2012 Heriot-Watt University
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
 *)

(** Parses the test answer JSON files, translucently constrained by refstruct{JSONPARSER}. *)
structure JsonParser : JSONPARSER =
struct

structure NJJP = JSONParser
structure EH = ErrorHandler
structure EK = ErrorKind
structure D  = Debug

(** A type which we use to represent a singular error, a record with seven fields.
 * Record fields are as follows:
 * \arg labels A tuple of an int and an int list.
 * \arg assumptions ?
 * \arg kind The error kind (#ErrorKind.kind).
 * \arg slice The program slice of the error.
 * \arg time Execution time.
 * \arg identifier Unique identifier of the error, (~1 if can't do that?).
 * \arg regions The regions attributed o the error (#ExtReg.regs).
 * *)
type oneerror = {labels       : int * int list,
		 assumptions  : LongId.keyOut list,
		 kind         : ErrorKind.kind,
		 slice        : string,
		 time         : LargeInt.int,
		 identifier   : int,
		 regions      : ExtReg.regs}

(** A type used to represent all errors in an input program, a record option ref with 13 fields.
 * \arg errors A list of errors that occured (#oneerror list).
 * \arg time A record containing fields 'analysis', 'enumeration', 'minimisation', 'slicing' and 'html', indicating time taken to run a test.
 * \arg tyvar A pair of an integer and an #Id.assocOut.
 * \arg ident An #Id.assocOut.
 * \arg constraint A record with three fields 'syntactic', 'top' and 'total'.
 * \arg labels A single integer (just the one?).
 * \arg minimisation A boolean indicating whether the slice is minimal.
 * \arg solution The solution that the test is used with. Now deprecated, and always set to 9.
 * \arg basis An integer corresponding to what level of basis was used (0 = none, 1 = internal, 2 = full).
 * \arg timelimit The timelimit specified for the test to run.
 * \arg labelling A string.
 * \arg final A boolean.
 * \arg name The name of the program. *)
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

(** Strips out a JSON object name and returns the value and the rest of the JSON object list. *)
fun getObject (JSON.OBJECT objectList) expectedString =
    let
	(** Returns the name inside a (name, value) JSON pair. *)
	fun getName (name, _)        = name
	(** Returns the vlue inside a (name, value) JSON pair. *)
	fun getValue (_, value) = value
	(** The top level object. *)
	val head = List.hd objectList
    in
	if (getName head) = expectedString
	then (getValue head, (JSON.OBJECT (List.tl objectList)))
	else raise EH.DeadBranch ("Format error with JSON test file (expected object not found)")
    end
  | getObject _ _                    = raise EH.DeadBranch ("Format error with JSON test file (getObject got something other than an object)")

(** Changes something of type JSON.STRING to a string. *)
fun getString    (JSON.STRING value) = value
  | getString      _                 = raise EH.DeadBranch ("Format error with JSON test file (getString got something other than a string)")

(** Parses a test file. *)
fun parseTest testfile =
    let
	(** Handles a JSON.NULL value. *)
	fun getNull      (JSON.NULL)         = ()
	  | getNull      _                   = raise EH.DeadBranch ("Format error with JSON test file (getNull got something other than NULL)")
	(** Returns the boolean in a JSON.BOOL value. *)
	fun getBool      (JSON.BOOL value)   = value
	  | getBool      _                   = raise EH.DeadBranch ("Format error with JSON test file (getBool got something other than a bool)")
	(** Returns the integer in a JSON.INT value, casting from IntInf. *)
	fun getInt       (JSON.INT value)    = (IntInf.toInt value:int)
	  | getInt      _                    = raise EH.DeadBranch ("Format error with JSON test file (getInt got something other than an int)")
	(** Returns the array in a JSON.ARRAY value. *)
	fun getIntArray  (JSON.ARRAY [])     = []
	  | getIntArray  (JSON.ARRAY (h::t)) = (getInt h::(getIntArray (JSON.ARRAY t)))
	  | getIntArray      _               = raise EH.DeadBranch ("Format error with JSON test file (getIntArray got something other than an array)")
	(** Returns the integer in a JSON.INT value. *)
	fun getIntInf    (JSON.INT value)    = value
	  | getIntInf      _                 = raise EH.DeadBranch ("Format error with JSON test file (getIntInf got something other than an int)")
	(** Returns the fload in a JSON.FLOAT value. *)
	fun getFloat     (JSON.FLOAT value)  = value
	  | getFloat      _                  = raise EH.DeadBranch ("Format error with JSON test file (getFloat got something other than a float)")
	(** Returns the string array in a JSON.ARRAY value. *)
	fun getStrArray  (JSON.ARRAY [])     = []
	  | getStrArray  (JSON.ARRAY (h::t)) = (getString h::(getStrArray (JSON.ARRAY t)))
	  | getStrArray      _               = raise EH.DeadBranch ("Format error with JSON test file (getStrArray got something other than an array)")

	(** Gets the next object in the JSON array. *)
	fun getNextObject (JSON.OBJECT([])) = raise EH.DeadBranch ("Format error with JSON test file (no next object)")
	  | getNextObject (JSON.OBJECT(((id,value)::t))) = (id, value)
	  | getNextObject _ = raise EH.DeadBranch ("Format error with JSON test file (getNextObject got something other than an object)")

	(** Finds a value with a given identifier in an object. *)
	fun findIdVal (JSON.OBJECT(object)) id =
	    let
		(** The object id of the next object in the JSON.OBJECT container. *)
		val (objectId, objectValue) = getNextObject (JSON.OBJECT(object))
	    in
		if id=objectId
		then objectValue
		else findIdVal (JSON.OBJECT(List.tl object)) id
	    end
	  | findIdVal _ _ = raise EH.DeadBranch ("Format error with JSON test file (findIdVal got something other than an object)")

	(** Gets the type variables stored in the JSON array. *)
	fun getTyvars    (JSON.ARRAY ([]))   = []
	  | getTyvars    (JSON.ARRAY (h::t)) = ((getInt(findIdVal h "id"), getString(findIdVal h "str"))::(getTyvars (JSON.ARRAY t)))
	  | getTyvars    _                   = raise EH.DeadBranch ("Format error with JSON test file (getTyvars got something other than an array)")

	(** Parses the regions stated in the test file. *)
	fun parseRegions (JSON.ARRAY []) = []
	  | parseRegions (JSON.ARRAY(h::t)) =
	    let
		(** Gets the file name of the test. *)
		val (fileName, rest) = getObject h "fileName"
		(** Gets the regions of the test. *)
		val (fileRegions, _) = getObject rest "regionList"

		(** Parses the file regions contained in the JSON file, contained in a JSON.ARRAY. *)
		fun parseFileRegions (JSON.ARRAY []) = []
		  | parseFileRegions (JSON.ARRAY(h::t)) =
		    let
			(** Gets the node type. *)
			val (nodeType, rest) = getObject h "nodeType"
			(** Gets the 'from' line. *)
			val (fromLine, rest) = getObject rest "fromLine"
			(** Gets the 'from' column. *)
			val (fromColumn, rest) = getObject rest "fromColumn"
			(** Gets the 'to' line. *)
			val (toLine, rest) = getObject rest "toLine"
			(** Gets the 'to' column. *)
			val (toColumn, rest) = getObject rest "toColumn"
			(** Gets the colour of the node. *)
			val (color, rest) = getObject rest "color"
			(** Gets the weight of the node. *)
			val (weight, rest) = getObject rest "weight"

			(** Translates the colour given in the test file to the ExtReg associated colour. *)
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

	(** Parses an error kind. *)
	fun parseKind object =
	    let
		(** Gets the error kind name. *)
		val (ek, rest) = getObject object "errorKindName"

		(** Gets the typename list. *)
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

		(** For the specerr list part of unmerr. *)
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
		  | "ErrorKind.EqTypeRequired" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.EqTypeRequired( (getInt(findIdVal errInfo "tnerrLabel1"), getInt(findIdVal errInfo "tnerrLabel2")))
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
		  | "ErrorKind.SharingTypeNotInSig" =>
		    let val (errInfo, rest) = getObject rest "errorKindInfo" in
			EK.SharingTypeNotInSig( getInt(findIdVal errInfo "label1"))
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
		  | "ErrorKind.TypeVarBind" =>
		    let val (errInfo, rest) = getObject rest "asmpOp" in
			if (getString(errInfo) = "NONE" handle EH.DeadBranch _ => false)
			then EK.TypeVarBind (NONE)
			else EK.TypeVarBind (SOME (getIntArray(findIdVal errInfo "synerrList"), getInt(findIdVal errInfo "synerrInt")))
		    end
		  | "ErrorKind.Warning" => EK.Warning(getString(findIdVal rest "warningStr"))
		  | "ErrorKind.Parsing" => EK.Parsing(getString(findIdVal rest "parsingStr"))
		  | "ErrorKind.RigidWhere" => EK.RigidWhere
		  | "ErrorKind.Inclusion"  => EK.Inclusion
		  | "ErrorKind.AppNotApp"  => EK.AppNotApp
		  | "ErrorKind.DiffFunName"  => EK.DiffFunName
		  | "ErrorKind.DiffNbArgFun"  => EK.DiffNbArgFun
		  | "ErrorKind.FreeTypeVarTop"  => EK.FreeTypeVarTop
		  | "ErrorKind.AsPatVar"  => EK.AsPatVar
		  | "ErrorKind.FnRecExp"  => EK.FnRecExp
		  | "ErrorKind.RealInPat" => EK.RealInPat
		  | "ErrorKind.FreeIdent" => EK.FreeIdent
		  | "ErrorKind.FreeOpen"  => EK.FreeOpen
		  | str => raise EH.DeadBranch ("Format error with JSON test file (unknown error kind: "^str^")")
	    end

	(** Parses the errors inside a test file. *)
	fun getErrors (JSON.ARRAY []) = (D.printDebug D.JSON D.PARSING (fn _ => "finished parsing errors object!"); [])
	  | getErrors (JSON.ARRAY(h::t)) =
	    let
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "parsing errors object. Getting a new error...")
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "getting the identifier...")
		val id  = getInt(findIdVal h "identifier")
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "getting the labels...")
		val lab = (getInt(findIdVal (findIdVal h "labels") "count"), getIntArray(findIdVal(findIdVal h "labels") "labelNumbers"))
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "getting the assumptions...")
		val assump = List.map (fn x => ([], x)) (getIntArray (findIdVal h "assumptions")) (* first part of tuple always empty list? *)
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "getting the kind...")
		(** The error kind. *)
		val k   = parseKind (findIdVal h "kind")
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "getting the slice...")
		(** The slice of the error. *)
		val sl  = getString(findIdVal h "slice")
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "getting the time...")
		(** The time taken to find the error. *)
		val tm  = getIntInf(findIdVal h "time")
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "getting the regions...")
		(** The regions of the error. *)
		val r   = parseRegions (findIdVal h "regions")
		val _   = D.printDebug D.JSON D.PARSING (fn _ => "done with this error")
	    in
		{identifier  = id,
		 labels      = lab,
                 assumptions = assump,
                 kind        = k,
                 slice       = sl,
                 time        = tm,
		 regions     = r}::(getErrors (JSON.ARRAY(t)))
	    end
	  | getErrors _ = raise EH.DeadBranch ("Format error with JSON test file (getErrors got something other than an array)")

	(** A value containing the result of calling the SML/NJ JSON parser on the test file. *)
	val test = NJJP.parseFile testfile handle _ => raise EH.DeadBranch ("Cannot parse JSON file: "^testfile^"\n")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "checking top-level objects of json file...")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'errors' object...")
	(** The errors object. *)
	val (errorList, test) = getObject test "errors"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'time' object...")
	(** The time object. *)
	val (timeObj, test) = getObject test "time"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'tyvar' object...")
	(** The type variable object. *)
	val (tyvarObj, test) = getObject test "tyvar"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'ident' object...")
	(** The idintefier object. *)
	val (identObj, test) = getObject test "ident"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'constraint' object...")
	(** The constraint object. *)
	val (constraintObj, test) = getObject test "constraint"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'labels' object...")
	(** The labels object. *)
	val (labels, test) = getObject test "labels"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'minimisation' object...")
	(** The minimisation object. *)
	val (minimisation, test) = getObject test "minimisation"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'solution' object...")
	(** \deprecated The solution object. *)
	val (solution, test) = getObject test "solution"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'basis' object...")
	(** The basis object. *)
	val (basis, test) = getObject test "basis"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'timelimit' object...")
	(** The timelimit object. *)
	val (timelimit, test) = getObject test "timelimit"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'labelling' object...")
	(** The labelling object. *)
	val (labelling, test) = getObject test "labelling"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting final object...")
	(** The 'final' object. *)
	val (final, test) = getObject test "final"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'name' object...")
	(** The name object. *)
	val (name, test) = getObject test "name"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "top level json objects correct!")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "parsing lower level objects...")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting all errors...")
	(** The errors inside the test file. *)
	val errs = getErrors errorList
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "skipping time, will get that at the end...")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting tyvar...")
	(** The type variables inside the test file. *)
	val tv = (getInt(findIdVal tyvarObj "tyvar"), getTyvars(findIdVal tyvarObj "assoc"))
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting ident...")
	(** The type variables inside the test file. *)
	val idnt = getTyvars(identObj)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting constraint...")
	(** The constraint. *)
	val cst = {total = getInt((findIdVal constraintObj "total")),
		   top = getInt((findIdVal constraintObj "top")),
		   syntactic = getInt((findIdVal constraintObj "syntactic"))}
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting labels...")
	(** The integer version of the label. *)
	val lab = getInt(labels)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting minimisation...")
	(** The boolean version of the minimisation state. *)
	val min = getBool(minimisation)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting solution...")
	(** The integer of the solution. *)
	val sol = getInt(solution)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting basis...")
	(** The integer of the basis used. *)
	val bas = getInt(basis)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting timelimit...")
	(** The timelimit. *)
	val tm = getIntInf(timelimit)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting labelling...")
	(** The labelling. *)
	val lbing = getString(labelling)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting final...")
	(** The boolean version of the 'final' field. *)
	val fin = getBool(final)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting name...")
	(** The name of the test. *)
	val nm = getString(name)
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "got everything, new getting time...")
    in
	ref (SOME {
	     errors = errs,
	     time = {analysis = getIntInf((findIdVal timeObj "analysis")),
		     enumeration = getIntInf((findIdVal timeObj "enumeration")),
		     minimisation = getIntInf((findIdVal timeObj "minimisation")),
		     slicing = getIntInf((findIdVal timeObj "slicing")),
		     html = getIntInf((findIdVal timeObj "html"))},
	     tyvar = tv, ident = idnt, constraint = cst, labels = lab,
	     minimisation = min, solution =sol , basis = bas, timelimit = tm, labelling = lbing,
	     final = fin, name = nm
	    })
    end

(** Parses the test control file which states where all the the tests live. *)
fun parseTestControlFile fileLocation =
    let
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "parsing test control file...")

	(** Gets string versions of the JSON object test paths stated in the control file. *)
	fun stripTestPaths (JSON.ARRAY([])) = []
	  | stripTestPaths (JSON.ARRAY(h::t)) = ((getString h)::(stripTestPaths (JSON.ARRAY t)))
	  | stripTestPaths _ = raise EH.DeadBranch ("JSON format incorrect for file: "^fileLocation^"\n")

	(** Holds result of calling the SML/N parser on the test control file. *)
	val testControl = NJJP.parseFile fileLocation handle _ => raise EH.DeadBranch ("Cannot parse test control JSON file: "^fileLocation^"\n")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "checking top-level objects of json file...")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting 'test-list' object...")
	(** The specified list of tests. *)
	val (testList, testControl) = getObject testControl "test-list"
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "top level json objects correct!")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "parsing lower level objects...")
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "getting test-list...")
	(** Holds the string versions of the test paths gathered by calling #stripTestPaths. *)
	val testPaths = stripTestPaths testList
	val _ = D.printDebug D.JSON D.PARSING (fn _ => "done!")
    in
	testPaths
    end
end
