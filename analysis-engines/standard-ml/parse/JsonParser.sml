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
		fun getName (name, _) = name
		fun getValue (_, value) = value
		val head = List.hd objectList
	    in
		if (getName head) = expectedString
		then (getValue head, (JSON.OBJECT (List.tl objectList)))
		else raise EH.DeadBranch ("Format error with JSON test file (expected object not found)")
	    end
	fun getNull      (JSON.NULL) = ()
	fun getBool      (JSON.BOOL value) = value
	fun getInt       (JSON.INT value) = (IntInf.toInt value:int)
	fun getIntArray  (JSON.ARRAY []) = []
	  | getIntArray  (JSON.ARRAY (h::t)) = (getInt h::(getIntArray (JSON.ARRAY t)))
	fun getIntInf    (JSON.INT value) = value
	fun getFloat     (JSON.FLOAT value) = value
	fun getString    (JSON.STRING value) = value
	fun getStrArray  (JSON.ARRAY []) = []
	  | getStrArray  (JSON.ARRAY (h::t)) = (getString h::(getStrArray (JSON.ARRAY t)))

	fun enumList valueList =
	    let
		fun enumListCount n [] = []
		  | enumListCount n (h::t) = (n,h)::(enumListCount (n+1) t)
	    in
		enumListCount 1 valueList
	    end

	fun getNextObject (JSON.OBJECT([])) = raise EH.DeadBranch ("Format error with JSON test file (no next object)")
	  | getNextObject (JSON.OBJECT(((id,value)::t))) = (id, value)

	(* finds a value with a given identifier in an object *)
	fun findIdVal (JSON.OBJECT(object)) id =
	    let
		val (objectId, objectValue) = getNextObject (JSON.OBJECT(object))
	    in
		if id=objectId
		then objectValue
		else findIdVal (JSON.OBJECT(List.tl object)) id
	    end

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

			fun getColor "blue"   = ExtReg.Blue
			  | getColor "red"    = ExtReg.Red
			  | getColor "purple" = ExtReg.Purple
			  | getColor "green"  = ExtReg.Green
			  | getColor "orange" = ExtReg.Orange
			  | getColor "yellow" = ExtReg.Yellow
		    in
			if getString(nodeType) = "node"
			then (ExtReg.N({from=(getInt(fromLine),getInt(fromColumn)),to=(getInt(toLine), getInt(toColumn))}, getColor(getString color), getInt(weight), parseFileRegions(findIdVal rest "regions"))::(parseFileRegions(JSON.ARRAY t)))
			else if getString(nodeType) = "head"
			then (ExtReg.H({from=(getInt(fromLine),getInt(fromColumn)),to=(getInt(toLine), getInt(toColumn))}, getColor(getString color), getInt(weight))::(parseFileRegions(JSON.ARRAY t)))
			else (ExtReg.L({from=(getInt(fromLine),getInt(fromColumn)),to=(getInt(toLine), getInt(toColumn))}, getColor(getString color), getInt(weight))::(parseFileRegions(JSON.ARRAY t)))
		    end
	    in
		(getString(fileName), parseFileRegions fileRegions)::(parseRegions(JSON.ARRAY t))
	    end

	fun getErrors (JSON.ARRAY []) = []
	fun getErrors (JSON.ARRAY(h::t)) = (parseRegions(findIdVal h "regions");
		[{identifier  = getInt(findIdVal h "identifier"),
		  labels      = (getInt(findIdVal (findIdVal h "labels") "count"), getIntArray(findIdVal(findIdVal h "labels") "labelNumbers")),
                  assumptions = List.map (fn x => ([], x)) (getIntArray (findIdVal h "assumptions")), (* this is a hack right now, lst in (lst, int) is always = [] *)
                  kind        = ErrorKind.FreeIdent,
                  slice       = getString(findIdVal h "slice"),
                  time        = getIntInf(findIdVal h "time"),
		  regions     = parseRegions (findIdVal h "regions")
		 }])

	val test = NJJP.parseFile testfile handle _ => raise EH.DeadBranch ("Cannot parse JSON file: "^testfile^"\n")
	val (errorList, test) = getObject test "errors"
	val (timeObj, test) = getObject test "time"
	val (tyvarObj, test) = getObject test "tyvar"
	val (constraintObj, test) = getObject test "constraint"
	val (labels, test) = getObject test "labels"
	val (minimisation, test) = getObject test "minimisation"
	val (solution, test) = getObject test "solution"
	val (basis, test) = getObject test "basis"
	val (timelimit, test) = getObject test "timelimit"
	val (labelling, test) = getObject test "labelling"
	val (final, test) = getObject test "final"
	val (name, test) = getObject test "name"

    in
	ref (SOME {
	     errors = (getErrors errorList),
	     time        = {analysis = getIntInf((findIdVal timeObj "analysis")),
			    enumeration = getIntInf((findIdVal timeObj "enumeration")),
			    minimisation = getIntInf((findIdVal timeObj "minimisation")),
			    slicing = getIntInf((findIdVal timeObj "slicing")),
			    html = getIntInf((findIdVal timeObj "html"))},
	     tyvar       = (getInt(findIdVal tyvarObj "tyvar"),
			    enumList(getStrArray(findIdVal tyvarObj "assoc"))),
	     ident       = enumList(getStrArray(findIdVal tyvarObj "assoc")),
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
