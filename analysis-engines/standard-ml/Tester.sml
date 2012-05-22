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
structure SOL = Solution
structure CDS = SplaySetFn(OrdStr)
structure JP  = JsonParser

type oneerror = JP.oneerror
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


(*datatype smlTesComment = USE  of string
		       | SBAS of string (* Set   Basis *)
		       | CBAS           (* Clear Basis *)
		       | NCOM           (* No Comment  *)*)

(* declaration of exceptions *)
exception FormatTest  of string (* The format of an error is not as it should be                               *)
exception MissingTest of string (* The slicer didn't find an error that was found before                       *)
exception CtxtDepTest of string (* Context dependencies are wrong for a slice                                  *)
exception FileEx                (* A file does not exist, it should always be caught                           *)
exception NoWrite               (* A file cannot be modified                                                   *)
exception BadTest               (* The slicer did not run                                                      *)
exception SlowTest              (* The slicer finds the same solutions as before but runs out of time          *)
exception BetterTest            (* The slicer rans out of time as before but finds more errors                 *)
exception TypableTest           (* The slicer found errors when it didn't before (test was marked as typable)  *)
exception ToomuchTest           (* The slicer found more errors than before when it shouldn't                  *)
exception NotmuchTest           (* The slicer didn't run out of time and didn't find as much errors as before  *)
exception RegsTest              (* Regions are wrong for a slice                                               *)
exception TocheckTest           (* A test has been recorded but the slices are not correct yet                 *)
exception NewerTest             (* The slicer currently uses an older SOL than the one used to record the test *)

val sep        = "    "
val emacstab   = "        "

val testFolder = ref "../../../testing/analysis-engine-tests/standard-ml"
val mytempfile = "/tmp/smltes-tmp"
val myfilebas  = ref "../../../lib/basis.sml"
val myfilehtml = "res.html"

(* This is the file in which the debugging info from running
 * the database checking is copied to. *)
val tmpdatabase = "/tmp/smltes-database"

(*val myfileerrors = "errors.sml"*)
val error : error = ref NONE
(* mytimelimit is the time limit used for the database
 - change to timelimitdb or something like that *)

(* nonmin is true if one wants to report non minimal errors
 * that then get minimised. *)
val nonmin = ref true
fun setNonMin b = nonmin := b

(* fullreport is true if we want to report all the information
 * in the XML output. *)
val fullreport = false

val mytimelimit = Int.toLarge 5000
val notimelimit = Int.toLarge ~1
val timelimit   = ref mytimelimit

fun settimelimit tm = timelimit := tm
fun gettimelimit _  = !timelimit

fun setstylecss n = H.setstylecss n

fun setsol   n  = SOL.setSol (SOL.fromInt n)
fun getsol   () = SOL.toInt (SOL.getSol ())
fun printsol () = print (SOL.toString () ^ "\n")

fun getTabSize () = R.getTabSize ()
fun setTabSize ts = R.setTabSize ts

(* functions to grab code and correct output from the test database *)
fun getfileerr  nb = (!testFolder) ^ "/test"   ^ Int.toString nb
fun getfilecode nb = (!testFolder) ^ "/code"   ^ Int.toString nb ^ ".sml"
fun getfilehtml nb = (!testFolder) ^ "/output" ^ Int.toString nb ^ ".html"

fun resetAll _ =
    (T.resetnexts   ();
     L.resetNext    ();
     I.resetIds     ();
     CL.resetClVar  ();
     EV.resetEnvVar ();
     ERR.resetError ())

fun consProgs filebas bbas filesin n ascid webdemo =
    P.consProgs (case bbas of 2 => [filebas] | _ => [])
		filesin
		n
		ascid
		bbas
		webdemo

val oneDummyTime = Int.toLarge 0
val dummyTimes   = (oneDummyTime,
		    oneDummyTime,
		    oneDummyTime,
		    oneDummyTime,
		    oneDummyTime)
val dummyName    = "dummy"
val initLab      = L.firstLab



(************************************************************)
(*                OUTPUTTING THE ERRORS                     *)
(************************************************************)

fun timesToStr (t1, t2, t3, t4, t5) =
    let val stt1 = "analyse=\""      ^ Int.toString (Int.fromLarge t1) ^ "ms\""
	val stt2 = "enum=\""         ^ Int.toString (Int.fromLarge t2) ^ "ms\""
 	val stt3 = "minimisation=\"" ^ Int.toString (Int.fromLarge t3) ^ "ms\""
	val stt4 = "slicing=\""      ^ Int.toString (Int.fromLarge t4) ^ "ms\""
	val stt5 = "html=\""         ^ Int.toString (Int.fromLarge t5) ^ "ms\""
    in (stt1, stt2, stt3, stt4, stt5)
    end

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

(*fun transfun1' #"\""    = "\""
  | transfun1' #"\\"    = ""
  | transfun1' #"\227"  = "\227"
  | transfun1' #"\128"  = "\128"
  | transfun1' #"\152"  = "\152"
  | transfun1' #"\153"  = "\153"
  | transfun1' #"\154"  = "\154"
  | transfun1' #"\155"  = "\155"
  | transfun1' #"\226"  = "\226"
  | transfun1' #"\167"  = "\167"
  | transfun1' #"\188"  = "\188"
  | transfun1' #"\189"  = "\189"
  | transfun1' x        = Char.toString x*)

fun transfun2 st = String.translate transfun1 st

(* quick hack for the next release to reduce overloading information from the basis *)
fun removeBasisSlice sl =
let
    fun stripBasisSlice sl =
	let
	    (* we start at two because when we look for the start of the basis slice, we take
             * two brackets into account *)
	    val bracketBalancer = ref 2;

	    (* locates the start of the slice for the basis *)
	    fun findBasisSlice [] = []
	      | findBasisSlice (h::t) =
		if h = #"\168" (* opening slice bracket *)
		then (bracketBalancer := !bracketBalancer + 1; findBasisSlice t)
		else
		    if h = #"\169" (* closing slice bracket *)
		    then (bracketBalancer := !bracketBalancer - 1;
			  if !bracketBalancer = 0
			  then t
			  else findBasisSlice t)
		    else findBasisSlice t
	in
	    String.implode (findBasisSlice sl)
	end

    fun findStartBasisSlice sl =
	if (String.extract (sl, 0, SOME(11))) = "..structure"
	   andalso
	   (String.extract (sl, 15, SOME(7))) = "..Basis"
	then stripBasisSlice (String.explode (String.extract (sl, 20, NONE)))
	else findStartBasisSlice (String.extract (sl, 1, NONE))
in
    findStartBasisSlice sl
end

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
    ((*D.printdebug2 (I.printAssoc ascid);*)
     H.transformErrSl fileout (#1 (P.convertToFull filebas NONE [])) errl ascid ast bhead true basisoverloading removeBasisSlice)

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
	 then sep ^ "<slice>"  ^ (removeBasisSlice sl) ^ "</slice>\n"
	 else sep ^ "<slice>"  ^ sl             ^ "</slice>\n" )      ^
	sep ^ "<regions>"      ^ reg            ^ "</regions>\n"      ^
	sep ^ "<minimal>"      ^ min            ^ "</minimal>\n"      ^
	begsep ^ "</error>\n"  ^
	errorsToXML xs begsep bslice basisoverloading
    end

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
        val sts = begsep ^ "<solution val=\"" ^ Int.toString (SOL.toInt (SOL.getSol ())) ^ "\"/>\n"
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

fun testXML fdebug =
    let val head = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    in head ^ "\n<example>\n" ^ fdebug sep ^ "</example>\n" end

fun buildError errl (ast, m, ascid) bmin times envContextSensitiveSyntaxPair initlab bfinal name bslice nenv _ =
    let fun toerrors [] = []
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
		  solution     = SOL.toInt (SOL.getSol ()),
		  timelimit    = gettimelimit (),
		  labelling    = ""(*A.printAstProgs ast*),
		  final        = bfinal,
		  name         = "\"" ^ name ^ "\""})
    end

fun assignError err = error := !err

fun errorsToSML [] _ _ _ = ""
  | errorsToSML [x] begsep bslice basisoverloading =
    let val (id, ll, sa, sk, tm, sl, re) = ERR.printOneSmlErr x bslice basisoverloading
	val err   = "{"          ^ id ^ ",\n" ^
		    begsep ^ " " ^ ll ^ ",\n" ^
		    begsep ^ " " ^ sa ^ ",\n" ^
		    begsep ^ " " ^ sk ^ ",\n" ^
		    (if String.isSubstring "Overload" sk andalso basisoverloading = 0
		     then (begsep ^ " " ^ "slice       = \"" ^ (removeBasisSlice sl) ^ ",\n")
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
		     then (begsep ^ " " ^ "slice       = \"" ^ (removeBasisSlice sl) ^ ",\n")
		     else (begsep ^ " " ^ sl ^ ",\n")) ^
		    begsep ^ " " ^ tm ^ ",\n" ^
		    begsep ^ " " ^ re ^ "}"
    in err ^ ",\n" ^ begsep ^ (errorsToSML xs begsep bslice basisoverloading)
    end

fun errorsToJSON [] _ _ _ = ""
  | errorsToJSON [x] begsep bslice basisoverloading =
    let val (id, ll, sa, sk, tm, sl, re) = ERR.printOneJsonErr x bslice basisoverloading
	val err   = "{"          ^ id ^ ",\n" ^
		    begsep ^ " " ^ ll ^ ",\n" ^
		    begsep ^ " " ^ sa ^ ",\n" ^
		    begsep ^ " " ^ sk ^ ",\n" ^
		    (if String.isSubstring "Overload" sk andalso basisoverloading = 0
		     then (begsep ^ " " ^ "\"slice\"       : \"" ^ (removeBasisSlice sl) ^ ",\n")
		     else (begsep ^ " " ^ sl ^ ",\n")) ^
		    begsep ^ " " ^ tm ^ ",\n" ^
		    begsep ^ " " ^ re ^ "}"
    in err
    end
  | errorsToJSON (x :: xs) begsep bslice basisoverloading =
    let val (id, ll, sa, sk, tm, sl, re) = ERR.printOneJsonErr x bslice basisoverloading
	val err   = "{"          ^ id ^ ",\n" ^
		    begsep ^ " " ^ ll ^ ",\n" ^
		    begsep ^ " " ^ sa ^ ",\n" ^
		    begsep ^ " " ^ sk ^ ",\n" ^
		    (if String.isSubstring "Overload" sk andalso basisoverloading = 0
		     then (begsep ^ " " ^ "\"slice\"       : \"" ^ (removeBasisSlice sl) ^ ",\n")
		     else (begsep ^ " " ^ sl ^ ",\n")) ^
		    begsep ^ " " ^ tm ^ ",\n" ^
		    begsep ^ " " ^ re ^ "}"
    in err ^ ",\n" ^ begsep ^ (errorsToJSON xs begsep bslice basisoverloading)
    end


fun convertErrors currentError newName =
    let

	fun printOneJsonErr {identifier, labels, assumptions, kind, slice, time, regions} =
	    let
		fun getFst (a,b) = a
		fun stringLabelList (a, [t]) = Int.toString t
		| stringLabelList (a,(h::t)) =
		    (Int.toString h)^", "^(stringLabelList (a, t))
		| stringLabelList _ = ""

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

	fun errorsToJSON2 [] = ""
	  | errorsToJSON2 [x] =
	    let val begsep = "                              "
		val (id, ll, sa, sk, tm, sl, re) = printOneJsonErr x
		val err   = "{"          ^ id ^ ",\n" ^
			    begsep ^ " " ^ ll ^ ",\n" ^
			    begsep ^ " " ^ sa ^ ",\n" ^
			    begsep ^ " " ^ sk ^ ",\n" ^
			    begsep ^ " " ^ sl ^ ",\n" ^
			    begsep ^ " " ^ tm ^ ",\n" ^
			    begsep ^ " " ^ re ^ "}"
	    in err
	    end
	  | errorsToJSON2 (x :: xs) =
	    let val begsep = "                  "
		val (id, ll, sa, sk, tm, sl, re) = printOneJsonErr x
		val err   = "{"          ^ id ^ ",\n" ^
			    begsep ^ " " ^ ll ^ ",\n" ^
			    begsep ^ " " ^ sa ^ ",\n" ^
			    begsep ^ " " ^ sk ^ ",\n" ^
			    begsep ^ " " ^ sl ^ ",\n" ^
			    begsep ^ " " ^ tm ^ ",\n" ^
			    begsep ^ " " ^ re ^ "}"
	    in err ^ ",\n" ^ begsep ^ (errorsToJSON2 xs)
	    end

	fun getTime {analysis, enumeration, minimisation, slicing, html} =
	    "{\"analysis\": "^(LargeInt.toString analysis)^", \"enumeration\": "^(LargeInt.toString enumeration)^", \"minimisation\": "^(LargeInt.toString minimisation)^
	    ", \"slicing\": "^(LargeInt.toString slicing)^", \"html\": "^(LargeInt.toString html)^"}"

	fun getConstraint {total, top, syntactic} =
	    "{\"total\": "^(Int.toString total)^", \"top\": "^(Int.toString top)^", \"syntactic\": "^(Int.toString syntactic)^"}"

	fun getTyvarNum  (a, _) = a
	fun getTyvarVals (_, b) = b

	fun getTyvar [] = ""
	  | getTyvar [(b,c)] = "{\"id\": " ^ (Int.toString b) ^ ", \"str\": \"" ^ c ^ "\"}"
	  | getTyvar ((b,c)::t) = "{\"id\": "^(Int.toString b)^", \"str\": \""^c^"\"},"^(getTyvar t)

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

fun generateTests min max =
    if min=max then ()
    else ((* use ("../../testing/analysis-engine-tests/standard-ml/test"^(Int.toString min)^".sml"); *)
	  (* convertErrors error ("test"^(Int.toString min)); *)
	  (* generateTests (min+1) max *));

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
	val sts = newsep ^ "solution     = " ^ Int.toString (SOL.toInt (SOL.getSol ()))
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
	val str = newsep ^ "\"errors\"       : [" ^ errorsToJSON errl errsep bslice basisoverloading ^ "]"
        val stb = newsep ^ "\"labelling\"    : \"" (*^ transfun2 (A.printAstProgs ast)*) ^ "\""
        val std = newsep ^ "\"minimisation\" : " ^ Bool.toString bmin
        val stu = newsep ^ "\"basis\"        : " ^ Int.toString nenv
	val sts = newsep ^ "\"solution\"     : " ^ Int.toString (SOL.toInt (SOL.getSol ()))
        val stt = newsep ^ "\"timelimit\"    : " ^ Int.toString (Int.fromLarge (gettimelimit ()))
        val stj = newsep ^ "\"final\"        : " ^ Bool.toString bfinal
        val stk = newsep ^ "\"name\"         : \"" ^ name  ^ "\""
        val stf = newsep ^ "\"time\"         : " ^
		  "{" ^
		  "\"analysis\": "     ^ Int.toString (Int.fromLarge t1) ^ ", " ^
		  "\"enumeration\": "  ^ Int.toString (Int.fromLarge t2) ^ ", " ^
		  "\"minimisation\": " ^ Int.toString (Int.fromLarge t3) ^ ", " ^
		  "\"slicing\": "      ^ Int.toString (Int.fromLarge t4) ^ ", " ^
		  "\"html\": "         ^ Int.toString (Int.fromLarge t5) ^
		  "}"
        val stg = newsep ^ "\"tyvar\"        : " ^
		  "{\"tyvar\": " ^ Int.toString (T.typeVarToInt (T.getTypeVar ()))
		  ^ ", \"assoc\": " ^ I.printJsonAssoc ascid ^ "}"
	val sti = newsep ^ "\"constraint\"   : " ^
		  "{" ^
		  "\"total\" : "     ^ Int.toString (EV.getnbcs envContextSensitiveSyntaxPair)      ^ ", " ^
		  "\"top\" : "       ^ Int.toString (EV.getnbcsttop envContextSensitiveSyntaxPair)  ^ ", " ^
		  "\"syntactic\" : " ^ Int.toString (EV.getnbcss envContextSensitiveSyntaxPair)     ^
		  "}"
	val stl = newsep ^ "\"labels\"       : " ^ Int.toString ((L.toInt m) - (L.toInt initlab))
	val st  = "{\n" ^
		  str ^ ",\n" ^
		  stf ^ ",\n" ^
		  stg ^ ",\n" ^
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
	then ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ "(slice . \"" ^ (removeBasisSlice sl) ^ "\n" ^ (*at ^ "\n" ^*) ap ^ "\n" ^ ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")"
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
	then ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ "(slice . \"" ^ (removeBasisSlice sl) ^ "\n" ^ at ^ "\n" ^ (*ap ^ "\n" ^*) ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")\n" ^ xs
	else ind ^ "(" ^ id ^ "\n" ^ re ^ "\n" ^ sl ^ "\n" ^ at ^ "\n" ^ (*ap ^ "\n" ^*) ek ^ "\n" ^ rm ^ "\n" ^ mn ^ ")\n" ^ xs
       )
    end

fun debuggingLISP errl (ast, m, ascid) bmin (t1, t2, t3, t4, t5) envContextSensitiveSyntaxPair initlab bfinal name bslice nenv basisoverloading _ =
    "(setq skalpel-slice-data '(" ^ (debuggingLISP' errl ascid "" bslice basisoverloading) ^ ")\n  )"

fun debuggingPERL' [] _ _ _ _ = ""
  | debuggingPERL' [err] ascid ind bslice basisoverloading =
    let val (id, ap, ek, rm, sl, re, mn) = ERR.printOnePerlErr err ascid bslice basisoverloading
    in "\n" ^ ind ^
       "{   "    ^ id ^
       ",\n    " ^ re ^
       (if String.isSubstring "OV" ek andalso basisoverloading = 0
	then (",\n    " ^ "slice     =>\"" ^ (removeBasisSlice sl))
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
	then (",\n    " ^ "slice     =>\"" ^ (removeBasisSlice sl))
	else (",\n    " ^ sl)) ^
       ",\n    " ^ ap ^
       ",\n    " ^ ek ^
       ",\n    " ^ rm ^
       ",\n    " ^ mn ^
       "}," ^ xs
    end
fun debuggingPERL errl (ast, m, ascid) bmin (t1, t2, t3, t4, t5) envcs initlab bfinal name bslice nenv basisoverloading _  =
    "my @error = (" ^ (debuggingPERL' errl ascid "" bslice basisoverloading) ^ ");\n" ^
    "sub getError { return @error; }"



(************************************************************)
(*                  RUNNING THE SLICER                      *)
(************************************************************)

fun getFileBasAndNum nenv filebas =
    (OS.FileSys.fileSize filebas;
     case nenv of
	 2 => (nenv, filebas)
       | _ => (nenv, ""))
    handle OS.SysErr (str, opt) => (case nenv of 2 => 1 | _ => nenv, "")

fun exportErrors [] _ _ _ _ counter = counter
  | exportErrors (error :: errors) funout time parse cs counter =
    (funout [error] parse cs counter time;
     exportErrors errors funout time parse cs (counter + 1))

fun slicing filebas filesin funout nenv webdemo bmin badmin bcs searchspace basisoverloading =

    let fun preSlicing funout filebas filesin nenv webdemo (preEnum, initEnum, runEnum) =
	    let val _ = resetAll ()
		val _ = print ("[Skalpel: parsing...]\n")
		val (progs, m, ascid, nenv) =
		    consProgs filebas nenv filesin initLab I.emAssoc webdemo
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
	    in (parse, envContextSensitiveSyntaxPair, errl3, filters, counter)
	    end

	fun initSlicing funout counter parse envContextSensitiveSyntaxPair found filters timerEnum (preEnum, initEnum, runEnum) =
	    let val export = if !nonmin then SOME funout else NONE
		val (errs1, found', filters', counter', continue) =
		    initEnum envContextSensitiveSyntaxPair found filters timerEnum parse export counter
		val found''   = found' (*ERR.recordSpeTreat found'*)
		val errs2     = errs1  (*ERR.getMergedErrors (err :: ERR.getNewErrors found' found'')*)
		(*(2010-07-05)We shouldn't need these 2 lines anymore because this was
		 * when record errors were not directly reported as merged errors. *)
		val time      = Int.fromLarge (VT.getMilliTime timerEnum)
		val counter'' = exportErrors errs2 funout time parse envContextSensitiveSyntaxPair counter'
	    in (found'', filters', counter'', continue)
	    end

	fun runSlicing funout counter parse envContextSensitiveSyntaxPair found filters timerEnum (preEnum, initEnum, runEnum) =
	    let val export    = if !nonmin then SOME funout else NONE
		val timelimit = gettimelimit ()
		val (errs1, found', filters', counter', continue) =
		    runEnum envContextSensitiveSyntaxPair found filters timelimit timerEnum parse export counter
		val found''   = found' (*ERR.recordSpeTreat found'*)
		val errs2     = errs1  (*ERR.getMergedErrors (err :: ERR.getNewErrors found' found'')*)
		(*(2010-07-05)Same as above concerning found'' and errs2.*)
		val time      = Int.fromLarge (VT.getMilliTime timerEnum)
		val counter'' = exportErrors errs2 funout time parse envContextSensitiveSyntaxPair counter'
	    (*val _ = print ("[" ^ Bool.toString continue ^ "]")*)
	    in if continue
	       then runSlicing funout counter'' parse envContextSensitiveSyntaxPair found'' filters' timerEnum (preEnum, initEnum, runEnum)
	       else (counter'', found'')
	    end

	fun loopSlicing filebas filesin funout nenv webdemo bmin (preEnum, initEnum, runEnum) =
	    let fun funout' errors parse cs counter time = funout errors parse false dummyTimes cs initLab dummyName "" counter time
		val _         = setNonMin bmin
		val timer     = VT.startTimer ()
		val (parse, envcs, errs, filters, counter) = preSlicing funout' filebas filesin nenv webdemo (preEnum, initEnum, runEnum)
		val timeCG    = VT.getMilliTime timer
		val timerEnum = VT.startTimer ()
		val (counter'', errors) =
		    case initSlicing funout' counter parse envcs errs filters timerEnum (preEnum, initEnum, runEnum) of
			(found', filters', counter', false) => (counter', found')
		      | (found', filters', counter', true)  =>
			runSlicing funout' counter' parse envcs found' filters' timerEnum (preEnum, initEnum, runEnum)
		val timeEN = VT.getMilliTime timer
		val time   = Int.fromLarge (VT.getMilliTime timerEnum)
		val _      = print ("[Skalpel: finished time=" ^ Int.toString time ^ "]\n")
	    in (counter'', errors, parse, envcs, timeCG, timeEN)
	    end

	val (counter, errors, parse, envContextSensitiveSyntaxPair as (env, css), timeCG, timeEN) =
	    (case searchspace of
		 1 => loopSlicing filebas filesin funout nenv webdemo bmin (EN1.preEnum, EN1.initEnum, EN1.runEnum)
	       | 2 => loopSlicing filebas filesin funout nenv webdemo bmin (EN2.preEnum, EN2.initEnum, EN2.runEnum)
	       | 3 => loopSlicing filebas filesin funout nenv webdemo bmin (EN3.preEnum, EN3.initEnum, EN3.runEnum)
	       | _ => loopSlicing filebas filesin funout nenv webdemo bmin (EN.preEnum,  EN.initEnum,  EN.runEnum))

    in if badmin
       then let val (nenv, filebas) = getFileBasAndNum nenv filebas
		val name    = "dummy"
		val times   = (timeCG, timeEN, timeEN, timeEN, timeEN)
		val dbgxml  = debuggingXML  errors parse bmin times envContextSensitiveSyntaxPair initLab false name true nenv basisoverloading
		val dbghtml = debuggingHTML errors parse bmin times envContextSensitiveSyntaxPair initLab false name true nenv basisoverloading removeBasisSlice
		val berr    = buildError    errors parse bmin times envContextSensitiveSyntaxPair initLab false name true nenv
		val _       = dbghtml myfilehtml filebas true ""
		val _       = print (testXML dbgxml)
		val _       = if bcs then print (Env.printEnv env "" ^ "\n") else ()
		val _       = assignError (berr "")
	    in counter
	    end
       else counter
    end


fun initialise filebas filesin nenv bprint =
    let val webdemo = false
	val _ = setNonMin false
        val timer = VT.startTimer ()
        (* Initialisation of all the variable counters *)
        val _ = resetAll ()
	val initlab = L.firstLab (* 0 is a dummy label *)
	(* Creation of the AST *)
	val _ = if bprint then print "creation of the ast...\n" else ()
	val (progs, m, ascid, nenv) =
	    consProgs filebas nenv filesin initlab I.emAssoc webdemo
	(*val _ = D.printdebug2 (I.printAssoc ascid)*)
	val parse = (progs, m, ascid)
	(*val stmin = TextIO.openIn filein
	val parse as (ast, m, (asc, ascid)) = P.f (filein, stmin) initlab
	    handle P.ParsingError (x, y) => dummyparsing x y initlab*)
        (*val _ = TextIO.closeIn stmin*)
	(*val _ = D.printdebug2 (A.printAstProgs progs)*)
	(*val _ = D.printdebug2 (PP.prettyPrintAst (A.getNonBasProgs progs))*)
	(*val _ = D.printdebug2 (PP.prettyPrintAst progs)*)
	val _ = L.setNextLab m
	(* Generation of the constraints *)
	val _ = if bprint then print "constraint generation...\n" else ()
        val envContextSensitiveSyntaxPair as (env, css) = AN.fullConsGen progs ascid nenv
	(*val _ = D.printdebug2 (EV.printEnv env "")*)
        val t1 = VT.getMilliTime timer
    in (initlab, parse, envContextSensitiveSyntaxPair, timer, t1)
    end

fun outTerm filebas errs ascid ast =
    let val _ = H2.transformErrSl "tmp" (#1 (P.convertToFull filebas NONE [])) errs ascid ast false false
	val _ = OS.Process.system ("IFSTMP=$IFS; IFS=$'\n'; for line in $(cat tmp); do echo -e \"$line\"; done; IFS=$IFSTMP")
    in () end

fun slicergen filebas filesin nenv bprint =
    let val (initlab,
	     parse as (ast, m, ascid),
	     envContextSensitiveSyntaxPair as (env, css),
	     timer,
	     t1) = initialise filebas filesin nenv bprint
	(* Enumeration of the type errors *)
	val _      = if bprint then print "enumeration...\n" else ()
        val (errl1, space) = EN.preEnum envContextSensitiveSyntaxPair parse
        val (errl2, bmin)  = EN.enum envContextSensitiveSyntaxPair errl1 space (gettimelimit ()) parse
        val t2     = VT.getMilliTime timer
	(* Minimisation *)
        val errl3  = errl2
        val t3     = VT.getMilliTime timer
	(* Ordering and merging of errors *)
	(*val _      = D.printdebug2 (">>" ^ Int.toString (List.length errl3) ^ "\n")*)
	(*val errl4  = ERR.recordSpeTreat  errl3*)
	(*val errl5  = ERR.getMergedErrors errl4*)
	val errl5  = errl3
	val errl6  = ERR.orderErrors     errl5
	val errl7  = errl6
	(* Slicing *)
        (*val errl8  = ERR.setSlices ast errl7*)
	val errl8  = errl7
        val t4     = VT.getMilliTime timer
	(* Creates the set of regions *)
	val bmerge = true
	(* use false for a better precision on the regions *)
	(* When changing bmerge to false, enable clW in Html.sml.
	 * It should be automatic, but this is still under construction.
	 * It's also possible to play with reorderFilters in Enumeration.sml. *)
	(*val errl9  = ERR.setRegs errl8 bmerge*)
	val errl9  = errl8
	val errl10 = if bmerge then errl9 else ERR.consWeight errl9 []
        val t5     = VT.getMilliTime timer
	(* Times *)
	val times = (t1, t2, t3, t4, t5)
	(*val _ = outTerm filebas errl10 ascid ast*)
    in SOME (errl10, parse, bmin, times, envContextSensitiveSyntaxPair, initlab)
    end



(************************************************************)
(*                MANAGING THE DATABASE                     *)
(************************************************************)

fun adderror filein nb bforce bfinal name nenv =
    let val fileout1 = getfileerr  nb
	val fileout2 = getfilecode nb
	val fileout3 = getfilehtml nb
	val a1    = (OS.FileSys.fileSize fileout1; false) handle SysErr => true
	val a2    = (OS.FileSys.fileSize fileout2; false) handle SysErr => true
	val a3    = (OS.FileSys.fileSize fileout3; false) handle SysErr => true
	val _     = if (a1 andalso a2 andalso a3) orelse bforce then () else raise FileEx
	val b1    = OS.FileSys.access (fileout1, [OS.FileSys.A_WRITE])
	val b2    = OS.FileSys.access (fileout2, [OS.FileSys.A_WRITE])
	val b3    = OS.FileSys.access (fileout3, [OS.FileSys.A_WRITE])
	val _     = if (b1 orelse a1) andalso (b2 orelse a2) andalso (b3 orelse a3) then () else raise NoWrite
	val stout = TextIO.openOut fileout2
	val stin  = TextIO.openIn filein
	val b     = ref true
	val _     = while !b do case TextIO.inputLine stin of
				    NONE      => b := false
				  | SOME line => TextIO.output (stout, line)
	val _     = TextIO.closeIn stin
	val _     = TextIO.closeOut stout
	val tmptm = gettimelimit ()
	val _     = settimelimit mytimelimit
	val _     = print "running...\n"
	val sl    = slicergen (!myfilebas) [fileout2] nenv false
	val _     = settimelimit tmptm
	val name' = case name of "" => Int.toString nb | _ => name
    in case sl of
	   SOME (errl, parse, bmin, times, envContextSensitiveSyntaxPair, initlab) =>
	   let val _     = debuggingHTML errl parse bmin times envContextSensitiveSyntaxPair initlab bfinal name' true  nenv 1 removeBasisSlice fileout3 (!myfilebas) true ""
	       val st    = debuggingSML  errl parse bmin times envContextSensitiveSyntaxPair initlab bfinal name' false nenv 1 "" (* this last argument is not used *)
	       val stout = TextIO.openOut fileout1
	       val _     = TextIO.output (stout, st)
	       val _     = TextIO.closeOut stout
	   in print ("Test " ^ Int.toString nb ^ " added\n")
	   end
	 | NONE => print "No slicing\n"
    end
    handle FileEx  => print "A test with the same number has already been recorded\n"
	 | NoWrite => print "This test cannot be recorded\n"

fun delerror nb =
    let val fileout1 = getfileerr  nb
	val fileout2 = getfilecode nb
	val fileout3 = getfilehtml nb
	val _        = OS.FileSys.remove fileout1
	val _        = OS.FileSys.remove fileout2
	val _        = OS.FileSys.remove fileout3
    in print ("Test " ^ Int.toString nb ^ " deleted\n")
    end
    handle SysErr => print "Cannot delete this test\n"

fun mverror nb1 nb2 bforce =
    let val fileout1  = getfileerr  nb1
	val fileout2  = getfilecode nb1
	val fileout3  = getfilehtml nb1
	val fileout1' = getfileerr  nb2
	val fileout2' = getfilecode nb2
	val fileout3' = getfilehtml nb2
	val a1        = (OS.FileSys.fileSize fileout1'; false) handle SysErr => true
	val a2        = (OS.FileSys.fileSize fileout2'; false) handle SysErr => true
	val a3        = (OS.FileSys.fileSize fileout3'; false) handle SysErr => true
	val _         = if (a1 andalso a2 andalso a3) orelse bforce then () else raise FileEx
	val b1        = OS.FileSys.access (fileout1', [OS.FileSys.A_WRITE])
	val b2        = OS.FileSys.access (fileout2', [OS.FileSys.A_WRITE])
	val b3        = OS.FileSys.access (fileout3', [OS.FileSys.A_WRITE])
	val _         = if (b1 orelse a1) andalso (b2 orelse a2) andalso (b3 orelse a3) then () else raise NoWrite
	val _         = OS.FileSys.rename {old = fileout1, new = fileout1'}
	val _         = OS.FileSys.rename {old = fileout2, new = fileout2'}
	val _         = OS.FileSys.rename {old = fileout3, new = fileout3'}
    in print ("Test " ^ (Int.toString nb1) ^ " moved to test " ^ (Int.toString nb2) ^ "\n")
    end
	handle FileEx  => print "A test with the same number has already been recorded\n"
	     | NoWrite => print "This test cannot be moved\n"

fun getTests _ =
    let fun stripnb file = String.substring (file, 4, (String.size file) - 4)
	    handle Subscript => raise EH.DeadBranch ""
	val dir = OS.FileSys.openDir (!testFolder)
	val b   = ref true
	val l   = ref (IntListSet.empty)
	val _   = while !b do case OS.FileSys.readDir dir of
				  SOME file =>  if String.isPrefix "test" file
						then (case Int.fromString (stripnb file) of
							 SOME nb => l := IntListSet.add (!l, nb)
						       | NONE    => ())
						else ()
				| NONE => b := false
	val _   = OS.FileSys.closeDir dir
    in IntListSet.listItems (!l)
    end
	handle SysErr => raise EH.DeadBranch "problem in reading tests\n"

fun printIntervals [] = ""
  | printIntervals ((x, y) :: xs) =
    "[" ^ Int.toString x ^
    "-" ^ Int.toString y ^
    "]" ^ printIntervals xs

fun findIntervals _ [] = []
  | findIntervals first [x] = [(first, x)]
  | findIntervals first (x :: y :: xs) =
    if y - x > 1
    then (first, x) :: (findIntervals y (y :: xs))
    else findIntervals first (y :: xs)

fun findIntervals' [] = []
  | findIntervals' (x :: xs) = findIntervals x (x :: xs)

fun listTests' msg tests =
    let val inter = findIntervals' tests
	val interst = printIntervals inter
    in print msg;
       print interst;
       print "\n"
    end

fun listTests _ = listTests' "Tests:" (getTests ())

fun getErrors _ =
    (#errors (Option.valOf (!error)))
    handle Option => raise FormatTest "errors"

fun getSlices _ =
    (map (fn x => #slice x) (#errors (Option.valOf (!error))))
    handle Option => raise FormatTest "slices"

fun getRegions _ =
    (map (fn x => #regions x) (#errors (Option.valOf (!error))))
    handle Option => raise FormatTest "regions"

fun getDependencies _ =
    (map (fn x => CD.inSet (#assumptions x)) (#errors (Option.valOf (!error))))
    handle Option => raise FormatTest "dependencies"

fun getMin _ =
    (#minimisation (Option.valOf (!error)))
    handle Option => raise FormatTest "min"

fun getFinal _ =
    (#final (Option.valOf (!error)))
    handle Option => raise FormatTest "final"

fun getTimeEnum _ =
    (#enumeration (#time (Option.valOf (!error))))
    handle Option => raise FormatTest "enumeration time"

fun getTimeCg _ =
    (#analysis (#time (Option.valOf (!error))))
    handle Option => raise FormatTest "constraint generation time"

fun getTimeLim _ =
    (#timelimit (Option.valOf (!error)))
    handle Option => raise FormatTest "time limit"

fun getBasis _ =
    (#basis (Option.valOf (!error)))
    handle Option => raise FormatTest "basis"

fun getAssoc _ =
    (#ident (Option.valOf (!error)))
    handle Option => raise FormatTest "assoc. list ids"

fun getSolution _ =
    (#solution (Option.valOf (!error)))
    handle Option => raise FormatTest "solution"

fun getLastRegs   _ = (getRegions ()) handle FormatTest _ => []
fun getLastSlices _ = (getSlices  ()) handle FormatTest _ => []
fun getLastMin    _ = (getMin     ()) handle FormatTest _ => false


(* thansform the old parentheses into the new ones *)
fun transParen #"\227"  = "\226" (* 1st old ldots and rdots *)
  | transParen #"\128"  = "\159"
  | transParen #"\154"  = "\168"
  | transParen #"\155"  = "\169"
  | transParen #"\226"  = "\226" (* 2nd old ldots and rdots *)
  | transParen #"\167"  = "\159"
  | transParen #"\188"  = "\168"
  | transParen #"\189"  = "\169"
  | transParen #"\159"  = "\159" (* new ldots and rdots *)
  | transParen #"\168"  = "\168"
  | transParen #"\169"  = "\169"
  | transParen #"\152"  = "\168" (* sequence dots *)
  | transParen #"\153"  = "\169"
  | transParen x        = Char.toString x

(* we want to replace "val ⧼..⧽" by "val " when it is not directly follows by "=" *)
(* look at that *)
(* (fn  (x, y) => (Substring.string x, Substring.string y)) (Substring.position "abc" (Substring.full "ghabcdef")); *)
(* (fn  (x, y) => (Substring.string x, Substring.string y)) (Substring.position "abc" (Substring.full "ghabbdef")); *)

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
	(*val _ = print ("1-" ^ slice ^ "\n")*)
	(*val _ = print ("2-" ^ newslice ^ "\n")*)
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
	(*val _ = D.printdebug2 (slice3)*)
    in (id, slice3, deps, regs) end

(* A slice is a string here *)
fun removeSlice _ [] = (NONE, [])
  | removeSlice x ((y as (id, slice : string, cds, regs)) :: xs) =
    if x = slice
    then (SOME y, xs)
    else (fn (u, v) => (u, y :: v)) (removeSlice x xs)

fun compareCDS cd1 cd2 =
    CDS.equal (CDS.addList (CDS.empty, cd1), CDS.addList (CDS.empty, cd2))

(* parameter 1 is the information in the database
 * parameter 2 is the imformation we found during execution *)
fun compareErrors2 [] [] (true,  id) = ()
  | compareErrors2 [] [] (false, id) = raise CtxtDepTest (Int.toString id)
  | compareErrors2 [] _  (true,  id) = raise BetterTest
  | compareErrors2 [] _  (false, id) = raise CtxtDepTest (Int.toString id)
  | compareErrors2 ((id, slice, cds, regs) :: xs) ys bid =
    case removeSlice slice ys of
	(SOME (id', slice', cds', regs'), ys') =>
	if compareCDS cds cds'
	then compareErrors2 xs ys' bid
	else compareErrors2 xs ys' (false, id)
      | (NONE, _)  => (D.printDebugFeature D.TEST D.TESTING ("cannot find slice: "^slice^ " in ["^
							    (List.foldr (op ^) "" ((List.map (fn (id, slice, cds, regs) => slice) ys)))
							   ^"]"); raise MissingTest (Int.toString id)) (* means new algo is less efficient or at least one error is different *)

fun compareErrors1 xs ys =
    let val xs1 = map (fn (i, x, y, z) => (i, String.translate transParen x, y, z)) xs
	val xs2 = map (fn x => upTos x) xs1
	(*val _  = map (fn x => D.printdebug2 (x ^ "\n")) xs3*)
	(*val xs4 = map upToSpaces xs3*)
	val ys1 = map (fn (i, x, y, z) => (i, String.translate transParen x, y, z)) ys
	val ys2 = map (fn x => upTos x) ys1
	(*val ys3 = map (fn (x, y, z) => (upToValTyVarSeq x "fun", y, z)) ys2*)
	(*val _  = map (fn x => D.printdebug2 (x ^ "\n")) ys3*)
	(*val _ = D.printdebug2 ("\n")*)
	(*val ys4 = map upToSpaces ys3*)
    in compareErrors2 xs2 ys2 (true, 0) end

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

(* declare functions which will give delevelopers messagees about the status of tests *)
fun messageFormat     nb st = "PROBLEM: test " ^ Int.toString nb ^ ": format (" ^ st ^ ")\n"
fun messageMissing    nb st = "PROBLEM: test " ^ Int.toString nb ^ ": a slice has not been found (Error UID = " ^ st ^ ")\n"
fun messageCtxtDep    nb st = "Test "          ^ Int.toString nb ^ " OK except the dependencies (" ^ st ^ ")\n"
fun messageTypable    nb    = "PROBLEM: test " ^ Int.toString nb ^ ": was marked as typable but is not anymore\n"
fun messageToomuch    nb    = "PROBLEM: test " ^ Int.toString nb ^ ": too many slices\n"
fun messageNotmuch    nb    = "PROBLEM: test " ^ Int.toString nb ^ ": not enough slices\n"
fun messageRegs       nb    = "PROBLEM: test " ^ Int.toString nb ^ ": the regions are wrong\n"
fun messageTocheck    nb    = "PROBLEM: test " ^ Int.toString nb ^ ": TO CHECK!!!!\n"
fun messageBad        nb    = "PROBLEM: test " ^ Int.toString nb ^ "\n"
fun messageBetter     nb    = "Test "          ^ Int.toString nb ^ " more than OK\n"
fun messageSlow       nb    = "Test "          ^ Int.toString nb ^ " OK but slower\n"
fun messageOK         nb    = "Test "          ^ Int.toString nb ^ " OK\n"
fun messageNewer      nb    = "PROBLEM: test " ^ Int.toString nb ^ ": test recorded with a newer SOL\n"
fun messageDeadBranch nb st = "PROBLEM: test " ^ Int.toString nb ^ ": ********DEADBRANCH(" ^ st ^ ")********\n"
fun messageTodo       nb    = "PROBLEM: test " ^ Int.toString nb ^ ": ********TODO********\n"

fun selectTests listtests =
    let val allTests  = getTests ()
	val someTests = IntListSet.listItems (IntListSet.addList (IntListSet.empty, listtests))
	fun intervTests x y =
	    let val (min, max) = if x < y then (x, y) else (y, x)
		val all = IntListSet.addList (IntListSet.empty, allTests)
		val red = IntListSet.map (fn x => if x > max orelse x < min then ~1 else x) all
		val dum = IntListSet.delete (red, ~1) handle LibBase.NotFound => red
	    in IntListSet.listItems dum
	    end
	val tests = case listtests of
			[]     => allTests
		      | [x, y] => if x <= y then intervTests x y else someTests
		      | _      => someTests
	val lastTest  = List.last tests handle Empty => 0
	val firstTest = List.hd tests   handle Empty => 1
    in (firstTest, lastTest, tests)
    end

fun generateTmpDBFile () =
    let val date = Date.toString (Date.fromTimeLocal (Time.now ()))
	val tr   = fn #" " => "_" | #":" => "-" | x => Char.toString x
    in tmpdatabase ^ "_" ^ String.translate tr date
    end

fun outputDB str stout =
    let val _ = print str
	val _ = TextIO.output (stout, str)
    in ()
    end

(* if listtests is []     then run all the tests                   *)
(* if listtests is [x, y] then run tests from min(x,y) to max(x,y) *)
(* otherwise run on the whole list                                 *)
fun checktests listtests =
    let val timerCheck = VT.startTimer ()
	val tmpfile    = generateTmpDBFile ()
	val stout      = TextIO.openOut tmpfile
	val _          = outputDB ("[begin database checking]\n") stout
	val (firsttest, lasttest, tests) = selectTests listtests
	val localtimelimit = mytimelimit
	val longest        = ref {test = firsttest, time = 0} (*longest test to check*)
	val tmptm          = gettimelimit ()
	val _              = settimelimit localtimelimit
	val oktests        = ref 0
	val badtests       = ref 0
	val checktests     = ref 0
	val todotests      = ref 0
	val deadtests      = ref 0
	val newertests     = ref 0
	val formtests      = ref 0
	val ctxttests      = ref 0
	fun plustest tests = tests := !tests + 1
	fun updLongest test time =
	    if time > #time (!longest)
	    then longest := {test = test, time = time}
	    else ()
	fun toErrsAndWarns1 errs =
	    List.partition
		(fn x : oneerror => case #kind x of EK.FreeIdent => false | _ => true)
		errs
	fun toErrsAndWarns2 errs =
	    List.partition
		(fn x => case ERR.getK x of EK.FreeIdent => false | _ => true)
		errs
	fun run nb [] = if nb <= lasttest
			then (outputDB ("No test " ^ Int.toString nb ^ "\n") stout; run (nb + 1) [])
			else ()
	  | run nb (x :: xs) =
	    if nb < x
	    then (case listtests of
		      [] => outputDB ("No test " ^ Int.toString nb ^ "\n") stout
		    | _  => ();
		  run (nb + 1) (x :: xs))
	    else if nb > x
	    then raise EH.DeadBranch ""
	    else ((* WordCBTHCSet.reset (); (* reset label set table *) *)
		  (* run the test *)
		  error := !(JP.parseTest (getfileerr nb));
		  (let

		       val errs1  = getErrors   ()
		       val bfinal = getFinal    ()
		       val tenum1 = getTimeEnum ()
		       val tcg1   = getTimeCg   ()
		       val tlim1  = getTimeLim  ()
		       val bas    = getBasis    ()
		       val assoc1 = getAssoc    ()
		       val sol1   = getSolution ()
		       val (errs1, warns1) = toErrsAndWarns1 errs1
		       val sls1   = map (fn x =>
					    (#identifier x,
					     #slice x,
					     CD.toStringList (CD.inSet (#assumptions x)) (I.inAssoc assoc1),
					     #regions x))
					errs1
		       val bend1  = tenum1 - tcg1 > tlim1 (* true if the timer ran off *)
		       val _      = if bfinal then () else raise TocheckTest
		       val fcode  = getfilecode nb
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
			      val _     = updLongest nb tenum2
			      val sol2  = SOL.toInt (SOL.getSol ())
			      val _     = if sol1 > sol2 then raise NewerTest else ()
			      (* we want to compare the slices and the context dependencies *)
			      val  _   = compareErrors (bend1, sls1) (bend2, sls2)
			  in plustest oktests; outputDB (messageOK nb) stout
			  end
			| NONE => raise BadTest
		   end
		   handle FormatTest    st => (plustest formtests;  outputDB (messageFormat     nb st) stout)
			| MissingTest   st => (plustest badtests;   outputDB (messageMissing    nb st) stout)
			| CtxtDepTest   st => (plustest ctxttests;  outputDB (messageCtxtDep    nb st) stout)
			| BadTest          => (plustest badtests;   outputDB (messageBad        nb)    stout)
			| SlowTest         => (plustest oktests;    outputDB (messageSlow       nb)    stout)
			| BetterTest       => (plustest oktests;    outputDB (messageBetter     nb)    stout)
			| TypableTest      => (plustest badtests;   outputDB (messageTypable    nb)    stout)
			| ToomuchTest      => (plustest badtests;   outputDB (messageToomuch    nb)    stout)
			| NotmuchTest      => (plustest badtests;   outputDB (messageNotmuch    nb)    stout)
			| RegsTest         => (plustest badtests;   outputDB (messageRegs       nb)    stout)
			| TocheckTest      => (plustest checktests; outputDB (messageTocheck    nb)    stout)
			| EH.DeadBranch st => (plustest deadtests;  outputDB (messageDeadBranch nb st) stout)
			| EH.TODO _          => (plustest todotests;  outputDB (messageTodo       nb)    stout)
			| NewerTest        => (plustest newertests; outputDB (messageNewer      nb)    stout));
		  run (nb + 1) xs)
	val _ = run firsttest tests
	val _ = settimelimit tmptm
	val endTime  = VT.milliToString "" timerCheck
	val longTest = Int.toString      (#test (!longest))
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
    in ()
    end

fun runtests listtests time =
    let val (firsttest, lasttest, tests) = selectTests listtests
	val localtimelimit = case time of SOME t => t | NONE => mytimelimit
	val tmptm     = gettimelimit ()
	val _         = settimelimit localtimelimit
	val tmst      = LargeInt.toString localtimelimit
	val timeE     = ref 0
	val nberr     = ref 0
	fun run nb [] = if nb <= lasttest
			then (print ("No test " ^ Int.toString nb ^ "\n"); run (nb + 1) [])
			else ()
	  | run nb (x :: xs) =
	    if nb < x
	    then (case listtests of
		      [] => print ("No test " ^ Int.toString nb ^ "\n")
		    | _  => ();
		  run (nb + 1) (x :: xs))
	    else
		if nb > x
		then raise EH.DeadBranch ""
		else (((case slicergen "" [(getfilecode nb)] 1 false of
			    SOME (errs, _, _, (tcg, tenum, _, _, _), _, _) =>
			    let val e    = length errs
				val t    = tenum - tcg
				val tst  = LargeInt.toString t
				val nbst = Int.toString nb
				val est  = Int.toString e
				val _    = nberr := e + !nberr
				val _    = timeE := t + !timeE
				val sbeg = "Test " ^ nbst
				val send = " - " ^ est ^ "\n"
			    in if t < localtimelimit
			       then print (sbeg ^ " < "  ^ tmst ^ " (" ^ tst ^ ")" ^ send)
			       else print (sbeg ^ " >= " ^ tmst ^ send)
			    end
			  | NONE => raise BadTest)
		       handle FormatTest st => print (messageFormat  nb st)
			    | BadTest       => print (messageBad     nb));
		      run (nb + 1) xs)
	val _ = run firsttest tests
	val _ = print ("Total:\n - slices: " ^ Int.toString (!nberr) ^
		       "\n - time:" ^ LargeInt.toString (!timeE) ^ "\n")
	val _ = settimelimit tmptm
    in ()
    end

fun printTypables _ =
    let val _ = PP.silence_compiler ()
	val tests = getTests ()
	val xs =
	    List.mapPartial
		(fn nb =>
		    let val _ = PP.use (getfileerr nb)
			    handle Error => error := NONE
			val errs = getLastSlices ()
			val typable = List.null errs
		    in if typable then SOME nb else NONE
		    end)
		tests
	val _ = PP.unsilence_compiler ()
    in listTests' "Typables:" xs
    end

fun removeRegions ast =
    let (*val _ = silence_compiler ()*)
	val st   = PP.prettyPrintAst ast
	(*val _    = D.printdebug2 ("(C)")*)
	val stat = OS.Process.system ("echo \"" ^ st ^ "\" | sed 's/from=([0-9]*,[0-9]*),to=([0-9]*,[0-9]*)/from=(0,0),to=(0,0)/g' > " ^ mytempfile)
	(*val _    = if OS.Process.isSuccess stat
		     then D.printdebug2 ("(OK)")
		     else D.printdebug2 ("(BAD)")*)
	(*val _    = D.printdebug2 ("(D)")*)
	val stm  = TextIO.openIn mytempfile
	fun consStr _ =
	    case TextIO.inputLine stm of
		NONE => ""
	      | SOME line => String.concatWith
				 " "
				 (String.tokens (fn #" " => true | _ => false) line)
			     ^ consStr ()
	val st'  = consStr ()
	(*val ast' = PP.readAst st'*)
	val _    = TextIO.closeIn stm
	(*val _ = unsilence_compiler ()*)
    in st'
    end


(* filein contains p1,
 * we create ast1 and p1, we now need to create ast2 from p2
 * we need to parse p2 to create an ast ast2 *)
fun checkAstFile filein =
    let val nasc = I.emAssoc
	val _    = resetAll ()
	val dp   = A.getDecPrint ()
	val _    = A.setDecPrint false
	val file = case #1 (P.convertToFull filein NONE []) of
		       NONE => raise EH.DeadBranch (filein ^ " does not exist")
		     | SOME f => f
	val str1 = TextIO.openIn file
	val (a1, n1, _) = P.parse "file1" str1 L.firstLab nasc
	    (*handle P.ParsingError (_, y) =>
		   P.dummyparsing y L.firstlab nasc*)
	val _    = TextIO.closeIn str1
	val ast1 = A.Progs [(a1, "", true, n1)]
	val p2   = A.printAstProgs ast1
	val _    = resetAll ()
	val str2 = TextIO.openString p2
	(*val _    = D.printdebug2 ("(A)" ^ p2)*)
	(* we can't use p2 because it contains extra stuff such as dots *)
	val (a2, n2, _) = P.parse "file2" str2 L.firstLab nasc
	    (*handle P.ParsingError (_, y) =>
		   P.dummyparsing y L.firstlab nasc*)
	(*val _    = D.printdebug2 ("(B)")*)
	val _    = TextIO.closeIn str1
	val ast2 = A.Progs [(a2, "", true, n2)]
	val _    = A.setDecPrint dp
	val stA  = removeRegions ast1
	val stB  = removeRegions ast1
    in (true, stA = stB)
    (* the first one was supposed to be the comparison between the 2 strings *)
    (* the asts are different because of the regions *)
    end
    handle _ => (print "problem"; (false, false))

fun checkAstDB _ =
    foldl
	(fn (nb, _) =>
	    let
		val file = getfilecode nb
	    in if #2 (checkAstFile file)
	       then print (file ^ " OK\n")
	       else print (file ^ " BAD\n")
	    end)
	()
	(getTests ())

fun vinnie nb =
    (PP.use (getfileerr nb);
     print ("-" ^ (Int.toString (#labels (Option.valOf (!error))) handle Option => "") ^ "\n"))

fun vinnie _ =
    let val set = L.ord [L.fromInt 1, L.fromInt 2, L.fromInt 3, L.fromInt 4]
	val (set1, set2) = L.splitIn2 set
	val _ = print (">>" ^ L.toString set1 ^ "\n>>" ^ L.toString set2)
    in ()
    end

(*fun vinnie _ = print (AstTest.genNewProgs ())*)

end
