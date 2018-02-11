(* Copyright 2009 2010 2011 2012 2013 Heriot-Watt University
 * Copyright 2018 Christian Gregg
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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Tester.sml
 *  o Description: Defines the structure Tester which contains the
 *      interface functions with the slicer, to test and run it.
 *      The structure Tester has signature TESTER.
 *)


structure Tester : TESTER = struct

(* shorten the name of some structures *)
structure A   = AstSML
structure P   = Parser
structure S   = Slicing
structure L   = Label
structure T   = Ty
structure I   = Id
structure R   = Reg
structure H   = Html(Tag)
structure H2  = Html(Tag2)
structure D   = Debug
structure AN  = Analyze
structure CD  = LongId
structure CL  = ClassId
structure ER  = ExtReg
structure EK  = ErrorKind
structure EN1 = Enum (SearchSpace)
structure EN2 = Enum (SearchSpace')
structure EN3 = Enum (SearchSpaceRbs)
structure EN  = EN1
structure EV  = Env
structure VT  = VTimer
structure PP  = Ppp
structure EH  = ErrorHandler
structure ERR = Error
structure CDS = SplaySetFn(OrdStr)
structure JP  = JsonParser

(** The type or a single error, set to be the same as #JsonParser.oneerror. *)
type oneerror = JP.oneerror

(** The type or an error, set to be the same as #JsonParser.error. *)
type error = JP.error

type 'a debug = ERR.error list ->
		A.packs        ->
		bool           ->
		ERR.times      ->
		EV.envContextSensitiveSyntaxPair      ->
		L.label        ->
		bool           ->
		string         ->
		bool           ->
		int            ->
		string         ->
		'a

    type 'a temptype = Error.error list ->
		    AstSML.packs     -> (* the int the next label w.r.t. progs *)
		    bool        -> (* true if minimiser was called (this not used anymore, because the minimiser is always called now) *)
		    Error.times ->
		    Env.envContextSensitiveSyntaxPair  ->
		    Label.label -> (* the first label in progs                                *)
		    bool        -> (* true if the slices look good enough                     *)
		    string         -> (* the name of the test                                    *)
		    bool        -> (* true if we want the pretty printing of slices           *)
		    int         -> (* basis switch: 0 (no basis), 1 (builtin basis), 2 (file) *)
		    int      -> (* basis overloading errors *)
		    string      -> (* initial indentation                                     *)
		    'a


(** Used when the format of an error is not as it should be. *)
exception FormatTest  of string
(** Raised when Skalpel didn't find an error that was found before. *)
exception MissingTest of string
(** Context dependencies are wrong for a slice. *)
exception CtxtDepTest of string
(** Raised when a file does not exist, it should always be caught. *)
exception FileEx
(* Raised when a file cannot be modified. *)
exception NoWrite
(** Raised when Skalpel did not run. *)
exception BadTest
(** Raised when Skalpel finds the same solutions as before but runs out of time. *)
exception SlowTest
(** Raised when Skalpel runs out of time but finds more errors. *)
exception BetterTest
(** Raised when Skalpel found errors that it did not before. *)
exception TypableTest
(** Raised when Skalpel found more errors than before when it shouldn't have. *)
exception ToomuchTest
(** Raised when Skalpel didn't run out of time and didn't find as much errors as before. *)
exception NotmuchTest
(** Raised when the regions are wrong for a slice. *)
exception RegsTest
(** Raised when a test has been recorded but the slices are not correct yet. *)
exception TocheckTest

(** A space separator (4 spaces). *)
val sep        = "    "
(** An emacs tab space separator (8 spaces). *)
val emacstab   = "        "

(** The location of the test folder by default. *)
val testFolder = ref "../../../testing/analysis-engine-tests/standard-ml"
(** The default location of the basis. *)
val myfilebas  = ref "../../../lib/basis.sml"
(** The default location of the HTML file for output. *)
val myfilehtml = "res.html"

(** This is the file in which the debugging info from running
 * the database checking is copied to. *)
val tmpdatabase = "/tmp/smltes-database"

(** Holds a JSON error (is this really used?). *)
val error : error = ref NONE

(** True if one wants to report non minimal errors that then get minimised. *)
val nonmin = ref true
(** Sets the nomin ref value to the argument specified. *)
fun setNonMin b = nonmin := b

(** True if we want to report all the information in the XML output. *)
val fullreport = false

(** Default time limit for Skalpel to run. *)
val mytimelimit = Int.toLarge 5000
(** Time set if there is to be no timelimit (set to -1). *)
val notimelimit = Int.toLarge ~1
(** Timelimit setting used for Skalpel, initially set to mytimelimit. *)
val timelimit   = ref mytimelimit

(** Sets the #timelimit value. *)
fun settimelimit tm = timelimit := tm
(** Gets the #timelimit value. *)
fun gettimelimit _  = !timelimit

(** Allows specification of the CSS style sheet. *)
fun setstylecss n = H.setstylecss n

(** Gets the tab size. *)
fun getTabSize () = R.getTabSize ()
(** Sets the tab size. *)
fun setTabSize ts = R.setTabSize ts

(** Gets the test files (solutions).
 * \deprecated 'Test' files no longer exist in the test database. *)
fun getfileerr  nb = (!testFolder) ^ "/test"   ^ Int.toString nb
(** Gets the code files. \depretcated *)
fun getfilecode nb = (!testFolder) ^ "/code"   ^ Int.toString nb ^ ".sml"
(** Gets the output files. \depretcated *)
fun getfilehtml nb = (!testFolder) ^ "/output" ^ Int.toString nb ^ ".html"

(** Resets the values for next labels, next identifiers etc. *)
fun resetAll _ =
    (T.resetnexts   ();
     L.resetNext    ();
     I.resetIds     ();
     CL.resetClVar  ();
     EV.resetEnvVar ();
     ERR.resetError ())

(** Calls #Parser.consProgs with the argument. *)
fun consProgs filebas bbas filesin n ascid webdemo =
    P.consProgs (case bbas of 2 => [filebas] | _ => [])
		filesin
		n
		ascid
		bbas
		webdemo

(** Used to repreesnt dummy times. *)
val oneDummyTime = Int.toLarge 0

(** A representation of dummy times. *)
val dummyTimes   = (oneDummyTime,
		    oneDummyTime,
		    oneDummyTime,
		    oneDummyTime,
		    oneDummyTime)

val dummyName    = "dummy"

(** Initial label, set to #Label.firstlab. *)
val initLab      = L.firstLab

(************************************************************)
(*                OUTPUTTING THE ERRORS                     *)
(************************************************************)

(** Prints the times found as a string. *)
fun timesToStr (t1, t2, t3, t4, t5) =
    let val stt1 = "analyse=\""      ^ Int.toString (Int.fromLarge t1) ^ "ms\""
	val stt2 = "enum=\""         ^ Int.toString (Int.fromLarge t2) ^ "ms\""
 	val stt3 = "minimisation=\"" ^ Int.toString (Int.fromLarge t3) ^ "ms\""
	val stt4 = "slicing=\""      ^ Int.toString (Int.fromLarge t4) ^ "ms\""
	val stt5 = "html=\""         ^ Int.toString (Int.fromLarge t5) ^ "ms\""
    in (stt1, stt2, stt3, stt4, stt5)
    end

(** Transforms a special character into a string. *)
fun transfun1 #"\""    = "\\\""
  | transfun1 #"\227"  = "\227" (* sequence ldots and rdots *)
  | transfun1 #"\128"  = "\128"
  | transfun1 #"\152"  = "\152"
  | transfun1 #"\153"  = "\153"
  | transfun1 #"\154"  = "\154"
  | transfun1 #"\155"  = "\155"
  | transfun1 #"\226"  = "\226" (* old ldots and rdots *)
  | transfun1 #"\167"  = "\167"
  | transfun1 #"\188"  = "\188"
  | transfun1 #"\189"  = "\189"
  | transfun1 #"\159"  = "\159" (* new ldots and rdots *)
  | transfun1 #"\168"  = "\168"
  | transfun1 #"\169"  = "\169"
  | transfun1 x        = Char.toString x

(** Uses String.translate to turn a character into a string. *)
fun transfun2 st = String.translate transfun1 st

(** Builds HTML representation of errors by calling #Html.transformErrSl. *)
fun debuggingHTML errl
		  (ast, m, ascid)
		  bmin
		  (t1, t2, t3, t4, t5)
		  envContextSensitiveSyntaxPair
		  initlab
		  bfinal
		  name
		  bslice
		  nenv
		  basisoverloading
		  removeBasisSlice
		  fileout
		  filebas
		  bhead
		  begsep =
    H.transformErrSl fileout (#1 (P.convertToFull filebas NONE [])) errl ascid ast bhead true basisoverloading removeBasisSlice

(** Prints errors in XML format. *)
fun errorsToXML [] _ _ _ = ""
  | errorsToXML (x :: xs) begsep bslice basisoverloading =
    let val (id, lab, statLab, builtBasis, assump, ek, rem, t, sl, reg, min) = ERR.printOneXmlErrTuple x (begsep ^sep) bslice basisoverloading
    in
	begsep ^ "<error>\n" ^
	sep ^ "<id>"           ^ id             ^ "</id>\n"           ^
	sep ^ "<labels nb=\""  ^ lab            ^ "</labels>\n"       ^
	sep ^ "<statusLabels>" ^ statLab        ^ "</statusLabels>\n" ^
	sep ^ "<builtinBasis>" ^ builtBasis     ^ "</builtinBasis>\n" ^
	sep ^ "<assumptions>"  ^ assump         ^ "</assumptions>\n"  ^
	sep ^ "<kind>"         ^ ek             ^ "</kind>\n"         ^
	sep ^ "<remove>"       ^ rem            ^ "</remove>\n"       ^
	sep ^ "<time>"         ^ t              ^ "</time>\n"         ^
	(if (String.isSubstring "overload" ek  andalso basisoverloading = 0)
	 then sep ^ "<slice>"  ^ (ERR.removeBasisSlice sl) ^ "</slice>\n"
	 else sep ^ "<slice>"  ^ sl             ^ "</slice>\n" )      ^
	sep ^ "<regions>"      ^ reg            ^ "</regions>\n"      ^
	sep ^ "<minimal>"      ^ min            ^ "</minimal>\n"      ^
	begsep ^ "</error>\n"  ^
	errorsToXML xs begsep bslice basisoverloading
    end

(** Builds errors in XML format. *)
fun debuggingXML errl
		 (ast, m, ascid)
		 bmin
		 (t1, t2, t3, t4, t5)
		 envContextSensitiveSyntaxPair
		 initlab
		 bfinal
		 name
		 bslice
		 nenv
		 basisoverloading
		 begsep =
    let val berr = begsep ^ "<errors nb=" ^ Int.toString (List.length errl) ^ ">\n"
	val eerr = begsep ^ "</errors>\n"
	val errs = berr ^ errorsToXML errl (begsep ^ sep) bslice basisoverloading ^ eerr
	val (stt1, stt2, stt3, stt4, stt5) = timesToStr (t1, t2, t3, t4, t5)
        (*val std = begsep ^ "<minimisation val=\"" ^ Bool.toString bmin ^ "\"/>\n"*)
        val stu = begsep ^ "<basis val=\"" ^ Int.toString nenv ^ "\"/>\n"
        val sts = begsep ^ "<solution val=\"9\"/>\n"
        val stt = begsep ^ "<timelimit val=\"" ^ Int.toString (Int.fromLarge (gettimelimit ())) ^ "\"/>\n"
        val stf = begsep ^ "<time " ^ stt1 ^ " " ^ stt2 ^ " " ^ stt3 ^ " " ^ stt4 ^ " " ^ stt5 ^ "/>\n"
        val stg = begsep ^ "<tyvar nb=\"" ^ Int.toString (T.typeVarToInt (T.getTypeVar ())) ^ "\"" ^
		  (if fullreport then " assoc=\"" ^ I.printAssoc ascid ^ "\"" else "") ^ "/>\n"
        val sth = begsep ^ "<ident" ^
		  (if fullreport then " assoc=\"" ^ I.printAssoc ascid ^ "\"" else "") ^ "/>\n"
	val sti = begsep ^ "<constraint total=\"" ^ Int.toString (EV.getnbcs envContextSensitiveSyntaxPair) ^
		  "\" top=\"" ^ Int.toString (EV.getnbcsttop envContextSensitiveSyntaxPair) ^
		  "\" syntactic=\"" ^ Int.toString (EV.getnbcss envContextSensitiveSyntaxPair) ^ "\"/>\n"
	val stl = begsep ^ "<labels nb=\"" ^ Int.toString ((L.toInt m) - (L.toInt initlab)) ^ "\"/>\n"
        val stj = begsep ^ "<final val=\"" ^ Bool.toString bfinal ^ "\"/>\n"
        val stk = begsep ^ "<name val=\""  ^ name ^ "\"/>\n"
        val stb = case nenv of
		      2 => "" (* We don't wanna print a long labeled term if using the basis. *)
		    | _ => begsep ^ "<labelling code=\"" ^ transfun2 (A.printAstProgs ast) ^ "\"/>\n"
	val st  = errs ^ stf ^ stg ^ sth ^ sti ^ stl ^ (*std ^*) sts ^ stu ^ stt ^ stb ^ stj ^ stk
    in st
    end

(** Builds an error (SML representation). *)
fun buildError errl (ast, m, ascid) bmin times envContextSensitiveSyntaxPair initlab bfinal name bslice nenv _ =
    let
	(** Helper function for #buildError. *)
	fun toerrors [] = []
	  | toerrors (err :: xs) =
	    {labels      = (L.length (ERR.getL err), L.toList (ERR.getL err)),
	     assumptions = CD.toOutList (ERR.getD err),
	     kind        = ERR.getK err,
	     slice       = S.printSlice (ERR.getS err) bslice,
	     time        = ERR.getT err,
	     identifier  = ERR.idToInt (ERR.getI err),
	     regions     = ERR.getR err} :: (toerrors xs)
	val times = (fn (t1, t2, t3, t4, t5) => {analysis     = t1,
						 enumeration  = t2,
						 minimisation = t3,
						 slicing      = t4,
						 html         = t5}) times
	val nbcs = {syntactic = EV.getnbcs     envContextSensitiveSyntaxPair,
		    top       = EV.getnbcsttop envContextSensitiveSyntaxPair,
		    total     = EV.getnbcss    envContextSensitiveSyntaxPair}
    in ref (SOME {errors       = toerrors errl,
		  time         = times,
		  tyvar        = (T.typeVarToInt (T.getTypeVar ()), I.outAssoc ascid),
		  ident        = I.outAssoc ascid,
		  constraint   = nbcs,
		  labels       = (L.toInt m) - (L.toInt initlab),
		  minimisation = bmin,
		  basis        = nenv,
		  solution     = 9,
		  timelimit    = gettimelimit (),
		  labelling    = ""(*A.printAstProgs ast*),
		  final        = bfinal,
		  name         = "\"" ^ name ^ "\""})
    end

(** Assigns the #error ref to the vaule in the argument. *)
fun assignError err = error := !err

(** Converts errors to SML format. *)
fun errorsToSML [] _ _ _ = ""
  | errorsToSML [x] begsep bslice basisoverloading =
    let val (id, ll, sa, sk, tm, sl, re) = ERR.printOneSmlErr x bslice basisoverloading
	val err   = "{"          ^ id ^ ",\n" ^
		    begsep ^ " " ^ ll ^ ",\n" ^
		    begsep ^ " " ^ sa ^ ",\n" ^
		    begsep ^ " " ^ sk ^ ",\n" ^
		    (if String.isSubstring "Overload" sk andalso basisoverloading = 0
		     then (begsep ^ " " ^ "slice       = \"" ^ (ERR.removeBasisSlice sl) ^ ",\n")
		     else (begsep ^ " " ^ sl ^ ",\n")) ^
		    begsep ^ " " ^ tm ^ ",\n" ^
		    begsep ^ " " ^ re ^ "}"
    in err
    end
  | errorsToSML (x :: xs) begsep bslice basisoverloading =
    let val (id, ll, sa, sk, tm, sl, re) = ERR.printOneSmlErr x bslice basisoverloading
	val err   = "{"          ^ id ^ ",\n" ^
		    begsep ^ " " ^ ll ^ ",\n" ^
		    begsep ^ " " ^ sa ^ ",\n" ^
		    begsep ^ " " ^ sk ^ ",\n" ^
		    (if String.isSubstring "Overload" sk andalso basisoverloading = 0
		     then (begsep ^ " " ^ "slice       = \"" ^ (ERR.removeBasisSlice sl) ^ ",\n")
		     else (begsep ^ " " ^ sl ^ ",\n")) ^
		    begsep ^ " " ^ tm ^ ",\n" ^
		    begsep ^ " " ^ re ^ "}"
    in err ^ ",\n" ^ begsep ^ (errorsToSML xs begsep bslice basisoverloading)
    end

(** Converts errors to JSON format. *)
fun errorsToJSON [] _ _ _ = ""
  | errorsToJSON [x] begsep bslice basisoverloading =
    let val (id, ll, sa, sk, tm, sl, re) = ERR.printOneJsonErr x bslice basisoverloading
	val err   = "{"          ^ id ^ ",\n" ^
		    begsep ^ " " ^ ll ^ ",\n" ^
		    begsep ^ " " ^ sk ^ ",\n" ^
		    begsep ^ " " ^ tm ^ ",\n" ^
		    (if String.isSubstring "Overload" sk andalso basisoverloading = 0
		     then (begsep ^ " " ^ "\"slice\"       : \"" ^ (ERR.removeBasisSlice sl) ^ ",\n")
		     else (begsep ^ " " ^ sl ^ ",\n")) ^
		    begsep ^ " " ^ sa ^ ",\n" ^
		    begsep ^ " " ^ re ^ "}"
    in err
    end
  | errorsToJSON (x :: xs) begsep bslice basisoverloading =
    let val (id, ll, sa, sk, tm, sl, re) = ERR.printOneJsonErr x bslice basisoverloading
	val err   = "{"          ^ id ^ ",\n" ^
		    begsep ^ " " ^ ll ^ ",\n" ^
		    begsep ^ " " ^ sk ^ ",\n" ^
		    begsep ^ " " ^ tm ^ ",\n" ^
		    (if String.isSubstring "Overload" sk andalso basisoverloading = 0
		     then (begsep ^ " " ^ "\"slice\"       : \"" ^ (ERR.removeBasisSlice sl) ^ ",\n")
		     else (begsep ^ " " ^ sl ^ ",\n")) ^
		    begsep ^ " " ^ sa ^ ",\n" ^
		    begsep ^ " " ^ re ^ "}"
    in err ^ ",\n" ^ begsep ^ (errorsToJSON xs begsep bslice basisoverloading)
    end

(** Builds error descriptions in JSON format. *)
fun convertErrors currentError newName =
    let

	(** Prints out a single error in JSON format. *)
	fun printOneJsonErr {identifier, labels, assumptions, kind, slice, time, regions} =
	    let
		(** Gets the first part of a tuple. *)
		fun getFst (a,b) = a
		(** A printer for labels. *)
		fun stringLabelList (a, [t]) = Int.toString t
		| stringLabelList (a,(h::t)) =
		    (Int.toString h)^", "^(stringLabelList (a, t))
		| stringLabelList _ = ""

		(** Prints the assumptions of an error. *)
		fun outputAssumptions [] = ""
		  | outputAssumptions [(_,value)] = Int.toString value
		  | outputAssumptions ((_,value)::t) = Int.toString value ^ ", " ^ (outputAssumptions t)

		val ll = "\"labels\"      : " ^ "{\"count\": " ^ (Int.toString (getFst labels)) ^ ", " ^ "\"labelNumbers\": [" ^ stringLabelList labels ^ "] }"
		val cd = "\"assumptions\" : [" ^ (outputAssumptions assumptions) ^ "]"
		val ek = "\"kind\"        : " ^ EK.printJsonErrKind kind
		val tm = "\"time\"        : " ^ LargeInt.toString time
		val id = "\"identifier\"  : " ^ Int.toString identifier
	    (* jpirie: we want to do it this way but the SML/NJ JSON lexer is broken. To fix! *)
	    (* val sl = "\"slice\"       : " ^ "\"" ^ (String.toString slice) ^ "\"" *)
		val sl = "\"slice\"       : " ^ "\"" ^ slice ^ "\""
	    in (id, ll, ek, tm, cd, sl, "\"regions\"     : " ^ "[" ^ ER.printJsonExtRegs regions ^ "]")
	    end

	(** Converts JSON errors to a string using #printOneJsonErr. *)
	fun errorsToJSON2 [] = ""
	  | errorsToJSON2 [x] =
	    let val begsep = "                              "
		val (id, labels, assumptions, kind, time, slice, regions) = printOneJsonErr x
		val err   = "{"          ^ id ^ ",\n" ^
			    begsep ^ " " ^ labels ^ ",\n" ^
			    begsep ^ " " ^ kind ^ ",\n" ^
			    begsep ^ " " ^ time ^ ",\n" ^
			    begsep ^ " " ^ slice ^ ",\n" ^
			    begsep ^ " " ^ assumptions ^ ",\n" ^
			    begsep ^ " " ^ regions ^ "}"
	    in err
	    end
	  | errorsToJSON2 (x :: xs) =
	    let val begsep = "                  "
		val (id, labels, assumptions, kind, time, slice, regions) = printOneJsonErr x
		val err   = "{"          ^ id ^ ",\n" ^
			    begsep ^ " " ^ labels ^ ",\n" ^
			    begsep ^ " " ^ kind ^ ",\n" ^
			    begsep ^ " " ^ time ^ ",\n" ^
			    begsep ^ " " ^ slice ^ ",\n" ^
			    begsep ^ " " ^ assumptions ^ ",\n" ^
			    begsep ^ " " ^ regions ^ "}"
	    in err ^ ",\n" ^ begsep ^ (errorsToJSON2 xs)
	    end

	(** Prints out the time taken during various points of execution of the test database. *)
	fun getTime {analysis, enumeration, minimisation, slicing, html} =
	    "{\"analysis\": "^(LargeInt.toString analysis)^", \"enumeration\": "^(LargeInt.toString enumeration)^", \"minimisation\": "^(LargeInt.toString minimisation)^
	    ", \"slicing\": "^(LargeInt.toString slicing)^", \"html\": "^(LargeInt.toString html)^"}"

	(** Converts the constraint field of an error to a string. *)
	fun getConstraint {total, top, syntactic} =
	    "{\"total\": "^(Int.toString total)^", \"top\": "^(Int.toString top)^", \"syntactic\": "^(Int.toString syntactic)^"}"

	(** Gets first part of the type variable information in an error. *)
	fun getTyvarNum  (a, _) = a
	(** Gets second part of the type variable information in an error. *)
	fun getTyvarVals (_, b) = b

	(** Given a list of ids and strings, will print them to a string. Used when printing the 'tyvar' portion of an error. *)
	fun getTyvar [] = ""
	  | getTyvar [(b,c)] = "{\"id\": " ^ (Int.toString b) ^ ", \"str\": \"" ^ c ^ "\"}"
	  | getTyvar ((b,c)::t) = "{\"id\": "^(Int.toString b)^", \"str\": \""^c^"\"},"^(getTyvar t)

	(** Helper function for #convertErrors *)
	fun convertSmlError (ref (SOME ({errors,
					 time,
					 tyvar,
					 ident,
					 constraint,
					 labels,
					 minimisation,
					 solution,
					 basis,
					 timelimit,
					 labelling,
					 final,
					 name})))
			    newname =
	    let
		val outstr = TextIO.openOut newname
	    in
	    (TextIO.outputSubstr (outstr, (Substring.full ("{\n" ^
	           "\"errors\"       : ["^ (errorsToJSON2 errors)^"],\n"^
		   "\"time\"         : "^(getTime time)^",\n"^
		   "\"tyvar\"        : {\"tyvar\": "^(Int.toString (getTyvarNum tyvar))^", \"assoc\": ["^(getTyvar (getTyvarVals tyvar))^"]},\n"^
		   "\"ident\"        : ["^(getTyvar ident)^"],\n"^
		   "\"constraint\"   : "^(getConstraint constraint)^",\n"^
		   "\"labels\"       : "^(Int.toString labels)^",\n"^
		   "\"minimisation\" : "^(Bool.toString minimisation)^",\n"^
		   "\"solution\"     : "^(Int.toString solution)^",\n"^
		   "\"basis\"        : "^(Int.toString basis)^",\n"^
		   "\"timelimit\"    : "^(IntInf.toString timelimit)^",\n"^
		   "\"labelling\"    : \""^labelling^"\",\n"^
		   "\"final\"        : "^(Bool.toString final)^",\n"^
		   "\"name\"         : \""^name^"\"\n}"))))
	    end
	  | convertSmlError (ref NONE) _ = raise EH.DeadBranch "convertSmlError received no error!"
    in
	convertSmlError currentError newName
    end

(** Builds errors in SML format. *)
fun debuggingSML errl
		 (ast, m, ascid)
		 bmin
		 (t1, t2, t3, t4, t5)
		 envContextSensitiveSyntaxPair
		 initlab
		 bfinal
		 name
		 bslice
		 nenv
		 basisoverloading
		 _ =
    let val tmpsep = "      "
	val newsep = emacstab ^ tmpsep
	val errsep = emacstab ^ emacstab ^ emacstab ^ tmpsep
	val str = newsep ^ "errors       = [" ^ errorsToSML errl errsep bslice basisoverloading ^ "]"
        val stb = newsep ^ "labelling    = \"" (*^ transfun2 (A.printAstProgs ast)*) ^ "\""
        val std = newsep ^ "minimisation = " ^ Bool.toString bmin
        val stu = newsep ^ "basis        = " ^ Int.toString nenv
	val sts = newsep ^ "solution     = 9"
        val stt = newsep ^ "timelimit    = " ^ Int.toString (Int.fromLarge (gettimelimit ()))
        val stj = newsep ^ "final        = " ^ Bool.toString bfinal
        val stk = newsep ^ "name         = \"" ^ name  ^ "\""
        val stf = newsep ^ "time         = " ^
		  "{" ^
		  "analysis = "     ^ Int.toString (Int.fromLarge t1) ^ ", " ^
		  "enumeration = "  ^ Int.toString (Int.fromLarge t2) ^ ", " ^
		  "minimisation = " ^ Int.toString (Int.fromLarge t3) ^ ", " ^
		  "slicing = "      ^ Int.toString (Int.fromLarge t4) ^ ", " ^
		  "html = "         ^ Int.toString (Int.fromLarge t5) ^
		  "}"
        val stg = newsep ^ "tyvar        = " ^
		  "(" ^
		  Int.toString (T.typeVarToInt (T.getTypeVar ())) ^
		  ", " ^
		  I.printAssoc' ascid ^
		  ")"
        val sth = newsep ^ "ident        = " ^ I.printAssoc' ascid
	val sti = newsep ^ "constraint   = " ^
		  "{" ^
		  "total = "     ^ Int.toString (EV.getnbcs envContextSensitiveSyntaxPair)      ^ ", " ^
		  "top = "       ^ Int.toString (EV.getnbcsttop envContextSensitiveSyntaxPair)  ^ ", " ^
		  "syntactic = " ^ Int.toString (EV.getnbcss envContextSensitiveSyntaxPair)     ^
		  "}"
	val stl = newsep ^ "labels       = " ^ Int.toString ((L.toInt m) - (L.toInt initlab))
	val st  = "val _ = Slicer.error :=\n" ^
		  emacstab ^ "SOME {\n" ^
		  str ^ ",\n" ^
		  stf ^ ",\n" ^
		  stg ^ ",\n" ^
		  sth ^ ",\n" ^
		  sti ^ ",\n" ^
		  stl ^ ",\n" ^
		  std ^ ",\n" ^
		  sts ^ ",\n" ^
		  stu ^ ",\n" ^
		  stt ^ ",\n" ^
		  stb ^ ",\n" ^
		  stj ^ ",\n" ^
		  stk ^ "\n"  ^
		  newsep ^ "}"
    in st
    end

(** Builds errors in JSON format. *)
fun debuggingJSON errl
		 (ast, m, ascid)
		 bmin
		 (t1, t2, t3, t4, t5)
		 envContextSensitiveSyntaxPair
		 initlab
		 bfinal
		 name
		 bslice
		 nenv
		 basisoverloading
		 _ =
    let val tmpsep = "      "
	val newsep = emacstab ^ tmpsep
	val errsep = emacstab ^ emacstab ^ emacstab ^ tmpsep
	val errors = newsep ^ "\"errors\"       : [" ^ errorsToJSON errl errsep bslice basisoverloading ^ "]"
        val labelling = newsep ^ "\"labelling\"    : \"" (*^ transfun2 (A.printAstProgs ast)*) ^ "\""
        val minimisation = newsep ^ "\"minimisation\" : " ^ Bool.toString bmin
        val basis = newsep ^ "\"basis\"        : " ^ Int.toString nenv
	val solution = newsep ^ "\"solution\"     : 9"
        val timelimit = newsep ^ "\"timelimit\"    : " ^ Int.toString (Int.fromLarge (gettimelimit ()))
        val final = newsep ^ "\"final\"        : " ^ Bool.toString bfinal
        val name = newsep ^ "\"name\"         : \"" ^ name  ^ "\""
        val time = newsep ^ "\"time\"         : " ^
		  "{" ^
		  "\"analysis\": "     ^ Int.toString (Int.fromLarge t1) ^ ", " ^
		  "\"enumeration\": "  ^ Int.toString (Int.fromLarge t2) ^ ", " ^
		  "\"minimisation\": " ^ Int.toString (Int.fromLarge t3) ^ ", " ^
		  "\"slicing\": "      ^ Int.toString (Int.fromLarge t4) ^ ", " ^
		  "\"html\": "         ^ Int.toString (Int.fromLarge t5) ^
		  "}"
        val tyvar = newsep ^ "\"tyvar\"        : " ^
		  "{\"tyvar\": " ^ Int.toString (T.typeVarToInt (T.getTypeVar ()))
		  ^ ", \"assoc\": " ^ I.printJsonAssoc ascid ^ "}"
	val ident = newsep ^ "\"ident\"        : " ^ I.printJsonAssoc ascid
	val constraint = newsep ^ "\"constraint\"   : " ^
		  "{" ^
		  "\"total\" : "     ^ Int.toString (EV.getnbcs envContextSensitiveSyntaxPair)      ^ ", " ^
		  "\"top\" : "       ^ Int.toString (EV.getnbcsttop envContextSensitiveSyntaxPair)  ^ ", " ^
		  "\"syntactic\" : " ^ Int.toString (EV.getnbcss envContextSensitiveSyntaxPair)     ^
		  "}"
	val labels = newsep ^ "\"labels\"       : " ^ Int.toString ((L.toInt m) - (L.toInt initlab))
	val errorString  = "{\n" ^
			   errors ^ ",\n" ^
			   time ^ ",\n" ^
			   tyvar ^ ",\n" ^
			   ident ^ ",\n" ^
			   constraint ^ ",\n" ^
			   labels ^ ",\n" ^
			   minimisation ^ ",\n" ^
			   solution ^ ",\n" ^
			   basis ^ ",\n" ^
			   timelimit ^ ",\n" ^
			   labelling ^ ",\n" ^
			   final ^ ",\n" ^
			   name ^ "\n"  ^
			   newsep ^ "}"
    in errorString
    end


(** Builds lisp in perl format, called by #debuggingLisp. *)
fun debuggingLISP' [] _ _ _ _ = ""
  | debuggingLISP' [err] ascid ind bslice basisoverloading =
    let val ind' = emacstab ^ "      "
	val (id, ap, ek, rm, sl, at, re, mn) = ERR.printOneLispErr err ascid bslice basisoverloading
	val ap = ind' ^ " " ^ ap
	val ek = ind' ^ " " ^ ek (* error kind *)
	val sl = ind' ^ " " ^ sl (* slice *)
	val at = ind' ^ " " ^ at
	val rm = ind' ^ " " ^ rm
	val re = ind' ^ " " ^ re (* regions *)
	val mn = ind' ^ " " ^ mn
    in (if String.isSubstring "OV" ek andalso basisoverloading = 0
	then ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ "(slice . \"" ^ (ERR.removeBasisSlice sl) ^ "\n" ^ (*at ^ "\n" ^*) ap ^ "\n" ^ ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")"
	else ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ sl ^ "\n" ^ (*at ^ "\n" ^*) ap ^ "\n" ^ ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")"
	)
    end (* at is the ast of the slice *)
  | debuggingLISP' (err :: errs) ascid ind bslice basisoverloading =
    let val ind' = emacstab ^ "      "
	val (id, ap, ek, rm, sl, at, re, mn) = ERR.printOneLispErr err ascid bslice basisoverloading
	val ap = ind' ^ " " ^ ap
	val ek = ind' ^ " " ^ ek
	val sl = ind' ^ " " ^ sl
	val at = ind' ^ " " ^ at
	val rm = ind' ^ " " ^ rm
	val re = ind' ^ " " ^ re
	val mn = ind' ^ " " ^ mn
	val xs = debuggingLISP' errs ascid ind' bslice basisoverloading
    in (if String.isSubstring "OV" ek andalso basisoverloading = 0
	then ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ "(slice . \"" ^ (ERR.removeBasisSlice sl) ^ "\n" ^ at ^ "\n" ^ (*ap ^ "\n" ^*) ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")\n" ^ xs
	else ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ sl ^ "\n" ^ at ^ "\n" ^ (*ap ^ "\n" ^*) ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")\n" ^ xs
       )
    end

(** Builds errors in LISP format. *)
fun debuggingLISP errl (ast, m, ascid) bmin (t1, t2, t3, t4, t5) envContextSensitiveSyntaxPair initlab bfinal name bslice nenv basisoverloading _ =
    "(setq skalpel-slice-data '(" ^ (debuggingLISP' errl ascid "" bslice basisoverloading) ^ ")\n  )"

(** Bulids errors in PERL format. *)
fun debuggingPERL' [] _ _ _ _ = ""
  | debuggingPERL' [err] ascid ind bslice basisoverloading =
    let val (id, ap, ek, rm, sl, re, mn) = ERR.printOnePerlErr err ascid bslice basisoverloading
    in "\n" ^ ind ^
       "{   "    ^ id ^
       ",\n    " ^ re ^
       (if String.isSubstring "OV" ek andalso basisoverloading = 0
	then (",\n    " ^ "slice     =>\"" ^ (ERR.removeBasisSlice sl))
	else (",\n    " ^ sl)) ^
       ",\n    " ^ ap ^
       ",\n    " ^ ek ^
       ",\n    " ^ rm ^
       ",\n    " ^ mn ^
       "}"
    end
  | debuggingPERL' (err :: errs) ascid ind bslice basisoverloading =
    let val (id, ap, ek, rm, sl, re, mn) = ERR.printOnePerlErr err ascid bslice basisoverloading
	val xs = debuggingPERL' errs ascid ind bslice basisoverloading
    in "\n" ^ ind ^
       "{   "    ^ id ^
       ",\n    " ^ re ^
       (if String.isSubstring "OV" ek andalso basisoverloading = 0
	then (",\n    " ^ "slice     =>\"" ^ (ERR.removeBasisSlice sl))
	else (",\n    " ^ sl)) ^
       ",\n    " ^ ap ^
       ",\n    " ^ ek ^
       ",\n    " ^ rm ^
       ",\n    " ^ mn ^
       "}," ^ xs
    end

(** Builds errors in Perl format by called #debuggingPERL'. *)
fun debuggingPERL errl (ast, m, ascid) bmin (t1, t2, t3, t4, t5) envcs initlab bfinal name bslice nenv basisoverloading _  =
    "my @error = (" ^ (debuggingPERL' errl ascid "" bslice basisoverloading) ^ ");\n" ^
    "sub getError { return @error; }"


(** Bulids errors in BASH format. *)
fun debuggingBASH' [] _ _ _ _ = ()
  | debuggingBASH' [err] ascid ind bslice basisoverloading =
    ERR.printOneBashErr err ascid bslice basisoverloading
  | debuggingBASH' (h::t) ascid ind bslice basisoverloading =
    (ERR.printOneBashErr h ascid bslice basisoverloading;
     debuggingBASH' t ascid ind bslice basisoverloading)

(** Bulids errors in BASH format by calling #debuggingBASH'. *)
fun debuggingBASH errl (ast, m, ascid) bmin (t1, t2, t3, t4, t5) envcs initlab bfinal name bslice nenv basisoverloading _  =
    (debuggingBASH' errl ascid "" bslice basisoverloading)

fun debuggingVIZ errl (ast, m, ascid) bmin (t1, t2, t3, t4, t5) envcs initlab bfinal name bslice nenv basisoverloading _ =
	let
		val error : ERR.error = List.hd errl
		(* builtInBasis : bool *)
		val builtInBasis = ERR.getB error
		(* dependencies : LongId.set *)
		val dependencies = ERR.getD error
		(* errorKind : ErrorKind.kind *)
		val errorKind = ERR.getK error
		(* id : Error.id *)
		val id = ERR.getI error
		(* labels : Label.labels *)
		val labels = ERR.getL error
		(* minimisationValue : true *)
		val minimisationValue = ERR.getM error
		(* regions : ExtReg.regs*)
		val regions = ERR.getR error
		(* removes : id list *)
		val removes = ERR.getE error
		(* boundFunction : Label.labels *)
		val boundFunctions = ERR.getF error
		(* slice : AstSML.progs*)
		val slice = ERR.getS error
		(* time : LargeInt.int*)
		val time = ERR.getT error
	in
		"{ \"id\":" ^ Int.toString (ERR.idToInt id) ^ "}"
	end

(************************************************************)
(*                  RUNNING THE SLICER                      *)
(************************************************************)

(** Gets the file to be used for the basis file (set to the empty string if the basis is not to be used). *)
fun getFileBasAndNum nenv filebas =
    (OS.FileSys.fileSize filebas;
     case nenv of
	 2 => (nenv, filebas)
       | _ => (nenv, ""))
    handle OS.SysErr (str, opt) => (case nenv of 2 => 1 | _ => nenv, "")

(** Calls the output function (funout) on each error and then returns how many applications to funout were made. *)
fun exportErrors [] _ _ _ _ counter = counter
 |  exportErrors (error::errors) funout time parse cs counter =
		let
			val _ = funout [error] parse cs counter time
		in exportErrors errors funout time parse cs (counter + 1) end

(** slicing function - a function to run the slicer. Arguments are as follows:
 * \param filebas file containing the basis
 * \param filesin list containing input files
 * \para funout Function used to export errors.
 * \param nenv see Analyze.sig for this integer - related to the basis
 * \param webdemo true if building a webdemo binary
 * \param bmin true if we want to report non-minimal errors
 * \param badmin true if we want to output errors in an html default file and in xml format in stdout
 * \param bcs true to print constraints in stdout (deprecated)
 * \param searchspace 1 for the default searchspace, 2 for the searchspace' and 3 for the rbs one, the default is 1
 * \param basisoverloading Value set to control overloading information in the basis.
 *)
fun slicing filebas filesin funout nenv webdemo bmin badmin bcs searchspace basisoverloading =
	let
		(** Calls functions to execute parsing, constraint generation, and enumeration. *)
		fun preSlicing funout filebas filesin nenv webdemo (preEnum, initEnum, runEnum) =
			let
				val _ = resetAll ()
				val _ = print ("[Skalpel: parsing...]\n")
				val (progs, m, ascid, nenv) = consProgs filebas nenv filesin initLab I.emAssoc webdemo
				val parse = (progs, m, ascid)
				val _ = L.setNextLab m
				val _ = print ("[Skalpel: constraint generation...]\n")
				val envContextSensitiveSyntaxPair = AN.fullConsGen progs ascid nenv
				val _ = print ("[Skalpel: enumeration...]\n")
				val (errl1, filters) = preEnum envContextSensitiveSyntaxPair parse
				val errl2 = ERR.setSlices progs errl1
				val errl3 = ERR.setRegs errl2 true
				val firstCounter = 1
				val counter = exportErrors errl3 funout 0 parse envContextSensitiveSyntaxPair firstCounter
			in (parse, envContextSensitiveSyntaxPair, errl3, filters, counter) end

		(** Helper function for #loopSlicing, calls #initEnum. *)
		fun initSlicing funout counter parse envContextSensitiveSyntaxPair found filters timerEnum (preEnum, initEnum, runEnum) =
			let
				val export = if !nonmin then SOME funout else NONE
				val (errs1, found', filters', counter', continue) = initEnum envContextSensitiveSyntaxPair found filters timerEnum parse export counter
				val time = Int.fromLarge (VT.getMilliTime timerEnum)
				val counter'' = exportErrors errs1 funout time parse envContextSensitiveSyntaxPair counter'
			in (found', filters', counter'', continue) end

		(** Helper function for #loopSlicing, runs the enumeration algorithm once. *)
		fun runSlicing funout counter parse envContextSensitiveSyntaxPair found filters timerEnum (preEnum, initEnum, runEnum) =
			let
				val export = if !nonmin then SOME funout else NONE
				val timelimit = gettimelimit ()
				val (errs1, found', filters', counter', continue) = runEnum envContextSensitiveSyntaxPair found filters timelimit timerEnum parse export counter
				val time = Int.fromLarge (VT.getMilliTime timerEnum)
				val counter'' = exportErrors errs1 funout time parse envContextSensitiveSyntaxPair counter'
			in
				if continue then
					runSlicing funout counter'' parse envContextSensitiveSyntaxPair found' filters' timerEnum (preEnum, initEnum, runEnum)
				else (counter'', found')
			end

		(** Loops the slicing algorithm by calling #preSlicing, #initSlicing, and #runSlicing in order to get a minimised error, then reports the errors found. *)
		fun loopSlicing filebas filesin funout nenv webdemo bmin (preEnum, initEnum, runEnum) =
			let
				(** Calls #funout. *)
				fun funout' errors parse cs counter time = funout errors parse false dummyTimes cs initLab dummyName "" counter time
				val _ = setNonMin bmin
				val timer = VT.startTimer ()
				val (parse, envcs, errs, filters, counter) = preSlicing funout' filebas filesin nenv webdemo (preEnum, initEnum, runEnum)
				val timeCG = VT.getMilliTime timer
				val timerEnum = VT.startTimer ()
				val _ = print ("[Skalpel: unification...]\n")
				val (counter'', errors) = case (initSlicing funout' counter parse envcs errs filters timerEnum (preEnum, initEnum, runEnum))
					of (found', filters', counter', false) => (counter', found')
					|  (found', filters', counter', true) => runSlicing funout' counter' parse envcs found' filters' timerEnum (preEnum, initEnum, runEnum)
				val timeEN = VT.getMilliTime timer
				val time = Int.fromLarge (VT.getMilliTime timerEnum)
				val _ = print ("Errors found: " ^ (Int.toString (List.length errors))^"\n")
				val _ = print ("[Skalpel: finished time=" ^ Int.toString time ^ "]\n")
			in
				(counter'', errors, parse, envcs, timeCG, timeEN)
			end

		val (counter, errors, parse, envContextSensitiveSyntaxPair as (env, css), timeCG, timeEN) = (case searchspace
			of 1 => loopSlicing filebas filesin funout nenv webdemo bmin (EN1.preEnum, EN1.initEnum, EN1.runEnum)
			 | 2 => loopSlicing filebas filesin funout nenv webdemo bmin (EN2.preEnum, EN2.initEnum, EN2.runEnum)
			 | 3 => loopSlicing filebas filesin funout nenv webdemo bmin (EN3.preEnum, EN3.initEnum, EN3.runEnum)
			 | _ => loopSlicing filebas filesin funout nenv webdemo bmin (EN.preEnum,  EN.initEnum,  EN.runEnum))

	in
		if badmin then
			let
				val (nenv, filebas) = getFileBasAndNum nenv filebas
				val name = "dummy"
				val times = (timeCG, timeEN, timeEN, timeEN, timeEN)
				val jsonOutput = debuggingJSON errors parse bmin times envContextSensitiveSyntaxPair initLab false name true nenv basisoverloading true
				val berr = buildError errors parse bmin times envContextSensitiveSyntaxPair initLab false name true nenv
				val _ = D.printDebug D.TEST D.TESTING (fn _ => jsonOutput)
				val _ = if bcs then print (Env.printEnv env "" ^ "\n") else ()
				val _ = assignError (berr "")
			in counter end
		else counter end


(** Bulids up the abstract syntax tree. *)
fun initialise filebas filesin nenv bprint =
let val webdemo = false
	val _ = setNonMin false
	val timer = VT.startTimer ()
	(* Initialisation of all the variable counters *)
	val _ = resetAll ()
	val initlab = L.firstLab (* 0 is a dummy label *)
	(* Creation of the AST *)
	val _ = if bprint then print "creation of the ast...\n" else ()
	val (progs, m, ascid, nenv) = consProgs filebas nenv filesin initlab I.emAssoc webdemo
	val parse = (progs, m, ascid)
	val _ = L.setNextLab m
	(* Generation of the constraints *)
	val _ = if bprint then print "constraint generation...\n" else ()
	val envContextSensitiveSyntaxPair as (env, css) = AN.fullConsGen progs ascid nenv
	val t1 = VT.getMilliTime timer
in (initlab, parse, envContextSensitiveSyntaxPair, timer, t1) end

(** Called when the user requests Skalpel to be run on a test database. *)
fun slicergen filebas filesin nenv bprint =
let
	val (initlab,parse as (ast, m, ascid), envContextSensitiveSyntaxPair as (env, css), timer, t1) = initialise filebas filesin nenv bprint
	val _ = if bprint then print "enumeration...\n" else ()
	val (errl1, space) = EN.preEnum envContextSensitiveSyntaxPair parse
	val (errl2, bmin) = EN.enum envContextSensitiveSyntaxPair errl1 space (gettimelimit ()) parse
	val t2 = VT.getMilliTime timer
	val t3 = VT.getMilliTime timer
	val errl6 = ERR.orderErrors     errl2
	val t4 = VT.getMilliTime timer
	val bmerge = true
	(* use false for a better precision on the regions *)
	(* When changing bmerge to false, enable clW in Html.sml.
	 * It should be automatic, but this is still under construction.
	 * It's also possible to play with reorderFilters in Enumeration.sml. *)
	val errl10 = if bmerge then errl6 else ERR.consWeight errl6 []
        val t5 = VT.getMilliTime timer
	(* Times *)
	val times = (t1, t2, t3, t4, t5)
in SOME (errl10, parse, bmin, times, envContextSensitiveSyntaxPair, initlab) end

(** Retrieves a list of test files.
 * \deprecated There should now be no used of this with the new JSON database. *)
fun getTests _ =
let
	(** Strips out a test number.
	 * Has a magic numbers 'bad smell'. We should get rid of that. *)
	fun stripnb file = String.substring (file, 4, (String.size file) - 4)
		handle Subscript => raise EH.DeadBranch ""
	val dir = OS.FileSys.openDir (!testFolder)
	val b   = ref true
	val l   = ref (IntListSet.empty)
	val _   = while !b do
		case (OS.FileSys.readDir dir)
		of SOME file =>  if (String.isPrefix "test" file) then
			(case (Int.fromString (stripnb file))
			of SOME nb => l := IntListSet.add (!l, nb)
			|  NONE    => ())
			else ()
		|  NONE => b := false
	val _   = OS.FileSys.closeDir dir
in IntListSet.listItems (!l) end
handle SysErr => raise EH.DeadBranch "problem in reading tests\n"

(** Prints the intervals between number as <N>-<N'>. *)
fun printIntervals [] = ""
 |  printIntervals ((x, y) :: xs) =
	"[" ^ Int.toString x ^
	"-" ^ Int.toString y ^
	"]" ^ printIntervals xs

(** Finds intervals in a list of numbers.. *)
fun findIntervals _ [] = []
 |  findIntervals first [x] = [(first, x)]
 |  findIntervals first (x :: y :: xs) =
	if y - x > 1 then
		(first, x) :: (findIntervals y (y :: xs))
	else findIntervals first (y :: xs)

(** Helper function for #findIntervals. *)
fun findIntervals' [] = []
 |  findIntervals' (x :: xs) = findIntervals x (x :: xs)

(** List the tests that we are to run Skalpel against. *)
fun listTests' msg tests =
let
	val inter = findIntervals' tests
	val interst = printIntervals inter
in print msg; print interst; print "\n" end

(** List the tests that we are to run Skalpel against by calling #listTests'. *)
fun listTests _ = listTests' "Tests:" (getTests ())

(** Returns the 'errors' portion of the #error record. *)
fun getErrors _ = (#errors (Option.valOf (!error)))
handle Option => raise FormatTest "errors"

(** Returns the 'slices' portion of the #error record. *)
fun getSlices _ = (map (fn x => #slice x) (#errors (Option.valOf (!error))))
handle Option => raise FormatTest "slices"

(** Returns the 'regions' portion of the #error record. *)
fun getRegions _ = (map (fn x => #regions x) (#errors (Option.valOf (!error))))
handle Option => raise FormatTest "regions"

(** Returns the 'dependencies' portion of the #error record. *)
fun getDependencies _ = (map (fn x => CD.inSet (#assumptions x)) (#errors (Option.valOf (!error))))
handle Option => raise FormatTest "dependencies"

(** Returns the 'min' portion of the #error record. *)
fun getMin _ = (#minimisation (Option.valOf (!error)))
handle Option => raise FormatTest "min"

(** Returns the 'final' portion of the #error record. *)
fun getFinal _ = (#final (Option.valOf (!error)))
handle Option => raise FormatTest "final"

(** Returns the enumeration time of the #error record. *)
fun getTimeEnum _ = (#enumeration (#time (Option.valOf (!error))))
handle Option => raise FormatTest "enumeration time"

(** Returns the analysis time of the #error record. *)
fun getTimeCg _ =
    (#analysis (#time (Option.valOf (!error))))
    handle Option => raise FormatTest "constraint generation time"

(** Returns the 'timelimit' portion of the #error record. *)
fun getTimeLim _ = (#timelimit (Option.valOf (!error)))
handle Option => raise FormatTest "time limit"

(** Returns the 'basis' portion of the #error record. *)
fun getBasis _ = (#basis (Option.valOf (!error)))
handle Option => raise FormatTest "basis"

(** Returns the 'ident' portion of the #error record. *)
fun getAssoc _ = (#ident (Option.valOf (!error)))
handle Option => raise FormatTest "assoc. list ids"

(** Calls #getRegions, handling the FormatTest exception. *)
fun getLastRegs   _ = (getRegions ()) handle FormatTest _ => []
(** Calls #getSlices, handling the FormatTest exception. *)
fun getLastSlices _ = (getSlices  ()) handle FormatTest _ => []
(** Calls #getMin, handling the FormatTest exception. *)
fun getLastMin    _ = (getMin     ()) handle FormatTest _ => false

(** Transform the old parentheses into the new ones. *)
fun transParen #"\227"  = "\226" (* 1st old ldots and rdots *)
 |  transParen #"\128"  = "\159"
 |  transParen #"\154"  = "\168"
 |  transParen #"\155"  = "\169"
 |  transParen #"\226"  = "\226" (* 2nd old ldots and rdots *)
 |  transParen #"\167"  = "\159"
 |  transParen #"\188"  = "\168"
 |  transParen #"\189"  = "\169"
 |  transParen #"\159"  = "\159" (* new ldots and rdots *)
 |  transParen #"\168"  = "\168"
 |  transParen #"\169"  = "\169"
 |  transParen #"\152"  = "\168" (* sequence dots *)
 |  transParen #"\153"  = "\169"
 |  transParen x        = Char.toString x

fun upToBasis slice =
    let val (dots, ldots, rdots, _, _) = S.getDots ()
	val prefBasis = "structure " ^ ldots ^ dots ^ "Basis :> sig " ^ ldots ^ dots
	val suffBasis = dots ^ rdots ^ " end = " ^ ldots ^ dots ^ rdots ^ dots ^ rdots ^ ";" ^
			dots ^ "open " ^ ldots ^ dots ^ "Basis" ^ dots ^ rdots
	val sizePrefBasis = String.size prefBasis
	val sizeSuffBasis = String.size suffBasis
	val (pref1, suff1) = (fn (x, y) => (Substring.string x, Substring.string y))
				 (Substring.position prefBasis (Substring.full slice))
	val slice1 = if String.isPrefix prefBasis suff1
		     then pref1 ^ String.substring (suff1, sizePrefBasis, (String.size suff1) - sizePrefBasis)
		     else slice
	val (pref2, suff2) = (fn (x, y) => (Substring.string x, Substring.string y))
				 (Substring.position suffBasis (Substring.full slice1))
	val slice2 = if String.isPrefix suffBasis suff2
		     then pref2 ^ String.substring (suff2, sizeSuffBasis, (String.size suff2) - sizeSuffBasis)
		     else slice1
    in slice2
    end

fun upToValTyVarSeq slice dec =
    let val (dots, ldots, rdots, _, _) = S.getDots ()
	val valpref = dec ^ " " ^ ldots ^ dots ^ rdots ^ " "
	val (pref, suff) = (fn (x, y) => (Substring.string x, Substring.string y))
			       (Substring.position valpref (Substring.full slice))
	val b = String.isPrefix valpref suff
	val newslice =
	    if b
	    then pref ^ dec ^ upToValTyVarSeq (String.substring (suff, 12, (String.size suff) - 12)) dec
	    else slice
    in newslice
    end

fun upToSpaces slice =
    let val parts = String.tokens (fn #"\n" => true
				    | #" "  => true
				    | #"\t" => true
				    | x     => false)
				  slice
    in String.concatWith " " parts
    end

fun upTos (id, slice0, deps, regs) =
    let val slice1 = upToValTyVarSeq slice0 "val"
	val slice2 = upToValTyVarSeq slice1 "fun"
	val slice3 = upToBasis slice2
    in (id, slice3, deps, regs) end

(** A slice is a string here. *)
fun removeSlice _ [] = (NONE, [])
  | removeSlice x ((y as (id, slice : string, cds, regs)) :: xs) =
    if x = slice
    then (SOME y, xs)
    else (fn (u, v) => (u, y :: v)) (removeSlice x xs)

(** Compares two context dependencies. *)
fun compareCDS cd1 cd2 =
    CDS.equal (CDS.addList (CDS.empty, cd1), CDS.addList (CDS.empty, cd2))

(** Prints context dependencies. *)
fun printCD [] = ""
  | printCD (h::[]) = ("\""^h^"\"")
  | printCD (h::t) = ("\""^h^"\","^(printCD t))

(** Parameter 1 is the information in the database,
 * Parameter 2 is the imformation we found during execution. *)
fun compareErrors2 [] [] (true,  id) = ()
  | compareErrors2 [] _  (true,  id) = raise BetterTest
  | compareErrors2 [] _  (false, id) = raise CtxtDepTest (Int.toString id)
  | compareErrors2 ((id, slice, cds, regs) :: xs) ys bid =
    case removeSlice slice ys of
	(* we found a slice, let's check the context dependancies *)
	(SOME (id', slice', cds', regs'), ys') =>
	if compareCDS cds cds'
	then compareErrors2 xs ys' bid           (* context dependancies are fine *)
	else (D.printDebug D.TEST D.TESTING (fn _ => "Difference in context dependancies. In database: "^(printCD cds)^". Discovered: "^(printCD cds')^".");
	      compareErrors2 xs ys' (false, id))   (* context dependancies are different *)
      (* we didn't find the slice, we failed this test *)
      | (NONE, _)  => (D.printDebug D.TEST D.TESTING (fn _ => "cannot find database slice:\n"^slice^ "\nfound in this execution:\n"^
							    (List.foldr (fn (x,y) => x^"\n"^y) "" ((List.map (fn (id, slice, cds, regs) => slice) ys))));
		       raise MissingTest (Int.toString id)) (* means new algo is less efficient or at least one error is different *)

fun compareErrors1 xs ys =
    let val xs1 = map (fn (i, x, y, z) => (i, String.translate transParen x, y, z)) xs
	val xs2 = map (fn x => upTos x) xs1
	val ys1 = map (fn (i, x, y, z) => (i, String.translate transParen x, y, z)) ys
	val ys2 = map (fn x => upTos x) ys1
    in compareErrors2 xs2 ys2 (true, 0) end

(** Compares errors for equality. *)
fun compareErrors (_, []) (_, (_ :: _)) = raise TypableTest
  | compareErrors (false, xs) (true, ys) =
    (* the timer didn't run off for the stored test but now ran off - which is problematic *)
    (case Int.compare (List.length xs, List.length ys) of
	 EQUAL   => (compareErrors1 xs ys; raise SlowTest) (* means that the new solution finds the same errors but is slower *)
       | LESS    => raise ToomuchTest
       | GREATER => compareErrors1 xs ys) (* too slow, an error is going to miss *)
  | compareErrors (false, xs) (false, ys) =
    (* the timer didn't run off *)
    (case Int.compare (List.length xs, List.length ys) of
	 EQUAL   => compareErrors1 xs ys
       | LESS    => raise ToomuchTest (* means no minimisation but not the same number of errors - one of the slicer is wrong *)
       | GREATER => raise NotmuchTest) (* means no minimisation but not the same number of errors - one of the slicer is wrong *)
  | compareErrors (true, xs) (_, ys) =
    (* the timed ran off for the stored test *)
    compareErrors1 xs ys (* means we now have to check that the new algo is more efficient *)

(** Returns a string that there was a format problem with a test. *)
fun messageFormat     test st = (#red (!D.colors)) ^ "PROBLEM: " ^  test ^ ": format (" ^ st ^ ")\n"
(** Returns a string that there is a missing test. *)
fun messageMissing    test st = (#red (!D.colors)) ^ "PROBLEM: " ^  test ^ ": a slice has not been found (Error UID = " ^ st ^ ")\n"
(** Returns a string that indicates there are context dependency discrepancies in tests. *)
fun messageCtxtDep    test st = (#purple (!D.colors)) ^ test ^ " OK except the dependencies (" ^ st ^ ")\n"
(** Returns a string that indicates a test was marked as typable but isn't any longer. *)
fun messageTypable    test    = (#red (!D.colors)) ^ "PROBLEM: " ^  test ^ ": was marked as typable but is not anymore\n"
(** Returns a string that indicates a test had too many slices generated for it. *)
fun messageToomuch    test    = (#red (!D.colors)) ^ "PROBLEM: " ^  test ^ ": too many slices\n"
(** Returns a string that indicates a test had not enough slices. *)
fun messageNotmuch    test    = (#red (!D.colors)) ^ "PROBLEM: " ^  test ^ ": not enough slices\n"
(** Returns a string that indicates the regions for a found error are wrong. *)
fun messageRegs       test    = (#red (!D.colors)) ^ "PROBLEM: " ^  test ^ ": the regions are wrong\n"
(** Returns a string that indicates a test has to yet get a confirmed solution and so cannot be checked. *)
fun messageTocheck    test    = (#yellow (!D.colors)) ^ "PROBLEM: " ^  test ^ ": TO CHECK!!!!\n"
(** Returns a string that indicates a test has an unspecified problem. *)
fun messageBad        test    = (#red (!D.colors)) ^ "PROBLEM: " ^  test ^ "\n"
(** Returns a string that indicates a test is more than OK. *)
fun messageBetter     test    = (#green (!D.colors)) ^  test ^ " more than OK\n"
(** Returns a string that indicates a test is OK but slower. *)
fun messageSlow       test    = (#green (!D.colors)) ^ test ^ " OK but slower\n"
(** Returns a string that indicates a test has successfully passed. *)
fun messageOK         test    = (#green (!D.colors)) ^ test ^ " OK\n"
(** Returns a string that indicates a test has dead branched. *)
fun messageDeadBranch test st = (#red (!D.colors)) ^ "PROBLEM: test " ^  test ^ ": ********DEADBRANCH(" ^ st ^ ")********\n"
(** Returns a string that indicates a test has raised the todo exception in #ErrorHandler. *)
fun messageTodo       test    = (#red (!D.colors)) ^ "PROBLEM: test " ^  test ^ ": TODO: "

(** Generates a temporary file used when checking the test database.
 * Might not still be needed... *)
fun generateTmpDBFile () =
let
	val date = Date.toString (Date.fromTimeLocal (Time.now ()))
	val tr   = fn #" " => "_" | #":" => "-" | x => Char.toString x
in tmpdatabase ^ "_" ^ String.translate tr date end

(** Outputs a line of text to the database output file. *)
fun outputDB str stout =
let
	val _ = print str
	val _ = TextIO.output (stout, str)
in () end

(** Function that handles the testing of Skalpel against the test database. *)
fun checktests listtests =
let
	val timerCheck = VT.startTimer ()
	(** Temporary file for output. *)
	val tmpfile    = generateTmpDBFile ()
	(** Create an output stream from tmpfile. *)
	val stout      = TextIO.openOut tmpfile
	val _          = outputDB ("[begin database checking]\n") stout
	val localtimelimit = mytimelimit
	(** The timelimit for the longest test. *)
	val longest        = ref {test = "", time = 0} (*longest test to check*)
	val tmptm          = gettimelimit ()
	val _              = settimelimit localtimelimit
	(** Number of OK tests. *)
	val oktests        = ref 0
	(** Number of bad tests. *)
	val badtests       = ref 0
	(** Numbered of tests without a confirmed solution. *)
	val checktests     = ref 0
	(** Number of tests that raised the TODO exception. *)
	val todotests      = ref 0
	val deadtests      = ref 0
	val newertests     = ref 0
	val formtests      = ref 0
	val ctxttests      = ref 0
	(** Increments the number of tests that have been run. *)
	fun plustest tests = tests := !tests + 1
	(** Updates the longest running test if applicable. *)
	fun updLongest test time =
	    if time > #time (!longest)
	    then longest := {test = test, time = time}
	    else ()

	(** Uses List.partition to separate errors into those which are free identifiers and the rest (using #kind to check). *)
	fun toErrsAndWarns1 errs =
	    List.partition
		(fn x : oneerror => case #kind x of EK.FreeIdent => false | _ => true)
		errs

	(** Uses List.partition to separate errors into those which are free identifiers and the rest (using #Error.getK to check). *)
	fun toErrsAndWarns2 errs =
	    List.partition
		(fn x => case ERR.getK x of EK.FreeIdent => false | _ => true)
		errs

	(** Parses the test control file. *)
	val testList = JP.parseTestControlFile (!(testFolder)^"/test-control")

	(** Function to run the tests on Skalpel. *)
	fun run [] = ()
	  | run (x :: xs) =
	    (* run the test *)
	    (* we use -4 because the length of the file extension, ".sml", is 4 *)
	    (error := !(JP.parseTest (!(testFolder)^"/"^(String.substring (x, 0, (String.size x) - 4))^"-solution"));
	    (let
		 val errs1  = getErrors   ()
		 val bfinal = getFinal    ()
		 val tenum1 = getTimeEnum ()
		 val tcg1   = getTimeCg   ()
		 val tlim1  = getTimeLim  ()
		 val bas    = getBasis    ()
		 val assoc1 = getAssoc    ()
		 val (errs1, warns1) = toErrsAndWarns1 errs1
		 val sls1   = map (fn x =>
				      (#identifier x,
				       #slice x,
				       CD.toStringList (CD.inSet (#assumptions x)) (I.inAssoc assoc1),
				       #regions x))
				  errs1
		 val bend1  = tenum1 - tcg1 > tlim1 (* true if the timer ran off *)
		 val _      = if bfinal then () else raise TocheckTest
		 val fcode  = (!(testFolder)^"/"^x)
		 val comp   = slicergen (!myfilebas) [fcode] bas false
	     in case comp of
		    SOME (errs2, (_, _, assoc2), _, (tcg2, tenum2, _, _, _), _, _) =>
		    let val (errs2, warns2) = toErrsAndWarns2 errs2
			val sls2  = map (fn x =>
					    (ERR.idToInt (ERR.getI x),
					     S.printSlice (ERR.getS x) false,
					     CD.toStringList (ERR.getD x) assoc2,
					     ERR.getR x))
					errs2
			val bend2 = tenum2 - tcg2 > localtimelimit
			val _     = updLongest x tenum2
			val sol2  = 9
			(* we want to compare the slices and the context dependencies *)
			val  _   = compareErrors (bend1, sls1) (bend2, sls2)
		    in plustest oktests; outputDB (messageOK x) stout
		    end
		  | NONE => raise BadTest
	     end
	     handle FormatTest    st => (plustest formtests;  outputDB (messageFormat     x st) stout)
		  | MissingTest   st => (plustest badtests;   outputDB (messageMissing    x st) stout)
		  | CtxtDepTest   st => (plustest ctxttests;  outputDB (messageCtxtDep    x st) stout)
		  | BadTest          => (plustest badtests;   outputDB (messageBad        x)    stout)
		  | SlowTest         => (plustest oktests;    outputDB (messageSlow       x)    stout)
		  | BetterTest       => (plustest oktests;    outputDB (messageBetter     x)    stout)
		  | TypableTest      => (plustest badtests;   outputDB (messageTypable    x)    stout)
		  | ToomuchTest      => (plustest badtests;   outputDB (messageToomuch    x)    stout)
		  | NotmuchTest      => (plustest badtests;   outputDB (messageNotmuch    x)    stout)
		  | RegsTest         => (plustest badtests;   outputDB (messageRegs       x)    stout)
		  | TocheckTest      => (plustest checktests; outputDB (messageTocheck    x)    stout)
		  | EH.DeadBranch st => (plustest deadtests;  outputDB (messageDeadBranch x st) stout)
		  | EH.TODO st          => (plustest todotests;  outputDB ((messageTodo       x)^st^"\n")    stout));
	    run xs)

	val _ = run testList
	val _ = settimelimit tmptm
	val endTime  = VT.milliToString "" timerCheck
	val longTest = #test (!longest)
	val longTime = LargeInt.toString (#time (!longest))
	val _ = outputDB ("[longest test: " ^ longTest ^ " (" ^ longTime ^ ")]\n") stout
	val _ = outputDB ("[OK      tests: " ^ Int.toString (!oktests)    ^ "]\n") stout
	val _ = outputDB ("[deps    tests: " ^ Int.toString (!ctxttests)  ^ "]\n") stout
	val _ = outputDB ("[failing tests: " ^ Int.toString (!badtests)   ^ "]\n") stout
	val _ = outputDB ("[tocheck tests: " ^ Int.toString (!checktests) ^ "]\n") stout
	val _ = outputDB ("[todo    tests: " ^ Int.toString (!todotests)  ^ "]\n") stout
	val _ = outputDB ("[dead    tests: " ^ Int.toString (!deadtests)  ^ "]\n") stout
	val _ = outputDB ("[format  tests: " ^ Int.toString (!formtests)  ^ "]\n") stout
	val _ = outputDB ("[sol     tests: " ^ Int.toString (!newertests) ^ "]\n") stout
	val _ = outputDB ("[end database checking (" ^ endTime ^ ")]\n") stout
	val _ = TextIO.closeOut stout
in () end

(** Prints the typeable tests. *)
fun printTypables _ = let
	val _ = PP.silence_compiler ()
	val tests = getTests ()
	val xs = List.mapPartial (fn nb => let
		val _ = PP.use (getfileerr nb) handle Error => error := NONE
		val errs = getLastSlices ()
		val typable = List.null errs
	in if typable then SOME nb else NONE end) tests
	val _ = PP.unsilence_compiler ()
in listTests' "Typables:" xs end
end
