(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 * Copyright 2011 Heriot-Watt University
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
 *  o Date:        21 May 2010
 *  o File name:   Env.sml
 *  o Description: Contains our constraint system.  The file defines the
 *      structure Env with signature ENV.
 *)


structure Env :> ENV = struct

(* ====== STRUCTURES ====== *)
structure T   = Ty
structure I   = Id
structure X   = Expans
structure L   = Label
structure P   = Poly
structure C   = ConsId
structure D   = Debug
structure CL  = ClassId
structure CD  = LongId
structure EL  = ExtLab
structure EH  = ErrorHandler
structure OME = SplayMapFn (OrdId)  (* map for Environments *)
structure OMC = SplayMapFn (OrdKey) (* map for Constraints, should be OrdLab *)
structure OMO = Fifo (*SplayMapFn (OrdLid) (* map for Open environments *)*)



(* ====== TYPES/DATATYPES ====== *)

(* ------ MAPPINGS ------ *)
type 'a cmap        = 'a OMC.map
type 'a emap        = 'a OME.map
type 'a omap        = 'a OMO.fifo

(* ------ VARIABLES ------ *)
type envvar         = int

(* ------ BINDERS ------ *)
type 'a bind  = 'a C.bind EL.extLab

(* ------ GENERIC ENVIRONMENT ------ *)
type 'a genv  = 'a bind list emap

(* ------ OPENENV ------ *)
datatype opnkind    = OST | DRE | ISI
type opnsem         = I.lid * L.label * opnkind
type opnenv         = opnsem omap

(* ------ VARENV ------ *)
type extvar         = T.ty bind
type varenv         = T.ty genv (* Tyty (ConsId.bind ExtLab.extLab list) OME.mp *)

(* ------ KIND OF A TYPE DECLARATION *)
datatype tnKind     = DAT | TYP

(* ------ TYPENV ------ *)
type exttyp         = (T.tyfun * tnKind * (varenv * bool) ref) bind
type typenv         = (T.tyfun * tnKind * (varenv * bool) ref) genv

(* ------ OVERLOADINGENV ------ *)
type extovc         = T.seqty bind
type ovcenv         = T.seqty genv

(* ------ TYVARENV ------ *)
type exttyv         = (T.tyvar * bool) bind
type tyvenv         = (T.tyvar * bool) genv

(* ------ INFORMATION ON ENVIRONMENT ------ *)
type tname          = {id : I.id, lab : L.label, kind : tnKind, name : T.tyname}
type tnmap          = tname list
datatype names      = TYNAME of tname EL.extLab | DUMTYNAME of tname | MAYTYNAME | NOTTYNAME of I.id EL.extLab
type infoEnv        = {lab : L.label, cmp : bool, tns : tnmap, fct : bool}

(* ------ ENRICHEMENT AND INSTANTIATION ------ *)
type evsbind        = envvar * envvar option * envvar * envvar option * L.label

(* ------ FUNCTOR INSTANTIATION ------ *)
type evfbind        = envvar * envvar * envvar * envvar * L.label

(* ------ SHARING ------ *)
type shabind        = envvar * envvar * envvar * L.label

(*(* ------ GENERALISATION OF EXPLICIT TYPE VARIABLES ------ *)
type tvsbind        = (T.tyvar * L.label) list
type exttv          = T.tyvar bind*)

(* ------ ACCESSORS ------ *)
type 'a accid       = {lid : I.lid, sem : 'a, class : CL.class, lab : L.label}

(* ------ LONG TYPE CONSTRUCTOR BINDER ------ *)
type longtyp        = T.tyfun accid EL.extLab

(*(* ------ CLASSES OF IDENTIFIERS ------ *)
datatype class      = VCL of CL.classvar
		    | CCL of CL.class * L.label * I.lid * bool*)

type class = CL.class

(* ------ SIGNATURE MATCHING ------ *)
datatype matchKind  = OPA (* Opaque      *)
		    | TRA (* Translucent *)

(* ------ ENVIRONMENT ------ *)
datatype env        = ENVCON of {vids : varenv,
				 typs : typenv,
				 tyvs : tyvenv,
				 strs : env genv,
				 sigs : env genv,
				 funs : (env * env) genv,
				 ovcs : ovcenv,
				 info : infoEnv}
		    | ENVVAR of envvar * L.label
		    | ENVSEQ of env * env
		    | ENVLOC of env * env
		    | ENVWHR of env * longtyp
		    | ENVSHA of env * env
		    | ENVSIG of env * env * matchKind
		    | ENVPOL of tyvenv * env
		    | ENVDAT of I.idl  * env
		    | ENVOPN of opnenv
		    | ENVDEP of env EL.extLab
		    | ENVFUN of cst
		    | ENVCST of cst
		    | ENVPTY of string
		    | ENVFIL of string * env * (unit -> env)
		    | ENVTOP

     and acc        = ACCVAR of T.ty        accid EL.extLab
		    | ACCETV of T.tyvar     accid EL.extLab
		    | ACCTYP of T.tyfun     accid EL.extLab
		    | ACCOVC of T.seqty     accid EL.extLab
		    | ACCSTR of env         accid EL.extLab
		    | ACCSIG of env         accid EL.extLab
		    | ACCFUN of (env * env) accid EL.extLab

     and ocst 	    = CSTTYP of (T.ty     * T.ty)     EL.extLab
		    | CSTTYN of (T.tnty   * T.tnty)   EL.extLab
		    | CSTSEQ of (T.seqty  * T.seqty)  EL.extLab
		    | CSTROW of (T.rowty  * T.rowty)  EL.extLab
		    | CSTLAB of (T.labty  * T.labty)  EL.extLab
		    | CSTENV of (env      * env)      EL.extLab
		    | CSTCLS of (CL.class * CL.class) EL.extLab
		    | CSTTYF of (T.tyfun  * T.tyfun)  EL.extLab
		    | CSTACC of acc
		    | CSTLET of env
		    | CSTSIG of evsbind
		    | CSTFUN of evfbind
		    | CSTSHA of shabind
     and cst        = OCST of ocst list cmap

type extstr = env bind
type strenv = env genv

type extsig = env bind
type sigenv = env genv

type funsem = env * env
type extfun = funsem bind
type funenv = funsem genv

datatype ocss       = CSSMULT of L.labels
		    | CSSCVAR of L.labels
		    | CSSEVAR of L.labels
		    | CSSECON of L.labels
		    | CSSINCL of L.labels
		    | CSSAPPL of L.labels
		    | CSSFNAM of L.labels
		    | CSSFARG of L.labels
		    | CSSTYVA of L.labels
		    | CSSLEFT of L.labels
		    | CSSFREC of L.labels
		    | CSSREAL of L.labels
		    | CSSFREE of L.labels
		    | CSSWARN of L.labels * string
		    | CSSPARS of L.labels * string
		    (*| CSB of csstyt*)
type css            = ocss list

type envcss = env * css


(* ====== FUNCTIONS ====== *)

(* ------ printing section ------ *)

val tab = "           "

fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printBind bind f assoc = EL.printExtLab bind (fn x => C.printBind x f assoc) assoc

fun printBind' bind f = printBind bind f I.emAssoc

fun printEnvVar v = "e" ^ Int.toString v

fun printEnvVarList xs = printlistgen xs printEnvVar


(*fun printStatus V = "V"
  | printStatus D = "D"
  | printStatus C = "C"
  | printStatus I = "I"
  | printStatus E = "E"

fun printIdLab (ID (id, lab, st)) ascid =
    "ID(" ^ I.printId   id  ^
    ","   ^ L.printelt  lab ^
    ","   ^ printStatus st  ^ ")"
  | printIdLab (LA lab) _ =
    "LA(" ^ L.printelt lab ^ ")"*)

fun printOp NONE _ = "-"
  | printOp (SOME x) f = f x

(*fun printDepsOp     x = printOp x L.toString
fun printStatusOp   x = printOp x L.printelt*)
fun printEvOp       x = printOp x printEnvVar
(*fun printConsEvOp   x = printOp x (fn x => C.printBind' x (fn _ => "()") printEnvVar)
(*fun printClStrEnvOp x = printOp x CL.printStrEnv*)

fun printAsmpOp  x ascid = printOp x (fn x => I.printLidSt x ascid)
fun printIdLabOp x ascid = printOp x (fn x => printIdLab   x ascid)*)

fun printTnKind DAT = "DAT"
  | printTnKind TYP = "TYP"

fun printGenEnv xs ind f =
    let val ind' = ind ^ "    "
    in #1 (OME.foldli
	       (fn (k, x, (y, z)) =>
		   (y ^ z ^ I.printId k ^ ":" ^ f x, "\n" ^ ind'))
	       ("", "")
	       xs)
    end

fun printExtVar extvar = printBind' extvar T.printty
and printExtTyv exttyv = printBind' exttyv (fn (tv, b) => "(" ^ T.printtyvar tv ^ "," ^ Bool.toString b ^ ")")
and printExtTyp exttyp = printBind' exttyp (fn (tyf, tnkind, cons) => "(" ^ T.printtyf tyf ^ "," ^ printTnKind tnkind ^ "," ^ printExtCon (!cons) ^ ")")
and printExtSeq extovc = printBind' extovc T.printseqty
and printExtCon (cons, b) = "(" ^ printVarEnv cons "" ^ "," ^ Bool.toString b ^ ")"

and printExtVarList xs = printlistgen xs printExtVar
and printExtTyvList xs = printlistgen xs printExtTyv
and printExtTypList xs = printlistgen xs printExtTyp
and printExtSeqList xs = printlistgen xs printExtSeq

and printTyvEnv xs ind = printGenEnv xs ind printExtTyvList
and printVarEnv xs ind = printGenEnv xs ind printExtVarList
and printTypEnv xs ind = printGenEnv xs ind printExtTypList
and printOvcEnv xs ind = printGenEnv xs ind printExtSeqList

(*fun printexplexptv xs =
    printlistgen
	xs
	(fn (x, nexp) => "(" ^ T.printtyvar x ^ "," ^ X.printnonexp nexp ^ ")")

fun printTvsBind xs =
    printlistgen
	xs
	(fn (x, lab) =>	"(" ^ T.printtyvar x ^ "," ^ L.printelt lab ^ ")")

fun printAccid {id, class, lab} =
    "(" ^ I.printLid  id    ^
    "," ^ CL.toString class ^
    "," ^ L.printelt  lab   ^ ")"

fun printClass (VCL cv) = "VCL(" ^ CL.printClassVar cv ^ ")"
  | printClass (CCL (cl, lab, lid, b)) =
    "CCL(" ^ CL.toString   cl  ^
    ","    ^ L.printelt    lab ^
    ","    ^ I.printLid    lid ^
    ","    ^ Bool.toString b   ^ ")"*)

fun printOpnKind OST = "OST"
  | printOpnKind DRE = "DRE"
  | printOpnKind ISI = "ISI"

fun printOpnEnv xs ind =
    #1 (OMO.foldl
	    (fn ((i, l, k), (y, z)) =>
		(y ^ z ^
		 I.printLid      i ^ ":(" ^
		 L.printLab      l ^ ","  ^
		 printOpnKind    k ^ ")",
		 "\n" ^ ind ^ "    "))
	    ("", "")
	    xs)

(*fun printOpnEnv' xs assoc =
    "[" ^ (#1 (OMO.foldl
		   (fn ((i, l, k, e), (y, z)) =>
		       (y ^ z ^
			"(" ^ I.printLid'    i assoc ^
			"," ^ L.printelt     l       ^
			"," ^ printOpnKind   k       ^ ")",
			","))
		   ("", "")
		   xs)) ^ "]"*)

fun printTnmap xs =
    printlistgen xs (fn {id, lab, kind, name} =>
			"(" ^ I.printId     id   ^
			"," ^ L.printLab    lab  ^
			"," ^ printTnKind   kind ^
			"," ^ T.printtyname name ^ ")")

(*fun printBindK LET = "LET"
  | printBindK DEC = "DEC"
  | printBindK FN  = "FN"*)

fun printNfoEnv {lab, cmp, tns, fct} =
    "(" ^ L.printLab    lab ^
    "," ^ Bool.toString cmp ^
    "," ^ printTnmap    tns ^
    "," ^ Bool.toString fct ^ ")"


(*fun printcsbsgen csenv assoc f1 f2 =
    printlistgen csenv (fn x => C.printBind x f1 f2 assoc)*)

fun printEvsBind (ev1, ev2, ev3, ev4, lab) =
    "(" ^ printEnvVar ev1 ^
    "," ^ printEvOp   ev2 ^
    "," ^ printEnvVar ev3 ^
    "," ^ printEvOp   ev4 ^
    "," ^ L.printLab  lab ^ ")"

fun printEvfBind (ev1, ev2, ev3, ev4, lab) =
    "(" ^ printEnvVar ev1 ^
    "," ^ printEnvVar ev2 ^
    "," ^ printEnvVar ev3 ^
    "," ^ printEnvVar ev4 ^
    "," ^ L.printLab  lab ^ ")"

fun printShaBind (ev1, ev2, ev3, lab) =
    "(" ^ printEnvVar ev1 ^
    "," ^ printEnvVar ev2 ^
    "," ^ printEnvVar ev3 ^
    "," ^ L.printLab  lab ^ ")"

fun printPair (x, y) f = "(" ^ f x ^ "," ^ f y ^ ")"

fun printAccId {lid, sem, class, lab} f ind ascid =
    "{" ^ I.printLid' lid ascid ^
    "," ^ f sem                 ^
    "," ^ CL.toString class     ^
    "," ^ L.printLab lab        ^ "}"

fun printMatchKind OPA = "OPA"
  | printMatchKind TRA = "TRA"

fun printLongTyp longtyp ind =
    "LONGTYP(" ^ EL.printExtLab' longtyp (fn x => printAccId x T.printtyf' ind I.emAssoc) ^ ")"

fun printExtEnv     x      = printBind' x (fn x => printEnv x "")
and printExtEnvList xs     = printlistgen xs printExtEnv
and printFunSemList xs     = printlistgen xs (fn (x, y) => "(" ^ printExtEnv x ^ "," ^ printExtEnv y ^ ")")
and printSigEnv     xs ind = printGenEnv xs (ind ^ "  ") printExtEnvList
and printStrEnv     xs ind = printGenEnv xs (ind ^ "  ") printExtEnvList
and printFunEnv     xs ind = printGenEnv xs ind printFunSemList
and printEnv (ENVCON {vids, typs, tyvs, strs, sigs, funs, ovcs, info}) ind =
    "\n" ^ ind ^ "{vids:" ^ printVarEnv vids ind ^
    "\n" ^ ind ^ " typs:" ^ printTypEnv typs ind ^
    "\n" ^ ind ^ " tyvs:" ^ printTyvEnv tyvs ind ^
    "\n" ^ ind ^ " strs:" ^ printStrEnv strs ind ^
    "\n" ^ ind ^ " sigs:" ^ printSigEnv sigs ind ^
    "\n" ^ ind ^ " ovcs:" ^ printOvcEnv ovcs ind ^
    "\n" ^ ind ^ " info:" ^ printNfoEnv info ^ "}"
  | printEnv (ENVVAR (ev, lab))  _   = "ENVVAR(" ^ printEnvVar ev ^ "," ^ L.printLab lab ^ ")"
  | printEnv (ENVOPN opnenv)     ind = "ENVOPN(" ^ printOpnEnv opnenv ind ^ ")"
  | printEnv (ENVDEP extenv)     ind = "ENVDEP(" ^ EL.printExtLab' extenv (fn env => printEnv env ind) ^ ")"
  | printEnv (ENVFUN cst) ind = "ENVCST(" ^ printcst' cst ind I.emAssoc ^ ")"
  | printEnv (ENVCST cst) ind = "ENVCST(" ^ printcst' cst ind I.emAssoc ^ ")"
  | printEnv (ENVSEQ (env1, env2)) ind =
    "ENVSEQ(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (ENVLOC (env1, env2)) ind =
    "ENVLOC(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (ENVWHR (env, longtyp)) ind =
    "ENVWHR(" ^ printEnv env ind ^ ",\n" ^ ind ^ printLongTyp longtyp ind ^ ")"
  | printEnv (ENVSHA (env1, env2)) ind =
    "ENVSHA(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (ENVSIG (env1, env2, kind)) ind =
    "ENVSIG(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ "," ^ printMatchKind kind ^ ")"
  | printEnv (ENVPOL (tyvenv, env)) ind =
    "ENVPOL(" ^ printTyvEnv tyvenv ind ^ ",\n" ^ ind ^ printEnv env ind ^ ")"
  | printEnv (ENVDAT (idl, env)) ind =
    "ENVDAT(" ^ I.printIdL idl ^ "," ^ printEnv env ind ^ ")"
  | printEnv (ENVPTY st) ind = "ENVPTY(" ^ st ^ ")"
  | printEnv (ENVFIL (st, env, stream)) ind =
    "ENVFIL(" ^ st ^ "," ^ printEnv env ind ^ ",\n" ^ printEnv (stream ()) ind ^ ")"
  | printEnv ENVTOP ind = "ENVTOP"
and printAcc (ACCVAR x) ind ascid =
    "ACCVAR(" ^ EL.printExtLab x (fn x => printAccId x T.printty' ind ascid) ascid ^ ")"
  | printAcc (ACCETV x) ind ascid =
    "ACCETV(" ^ EL.printExtLab x (fn x => printAccId x T.printtyvar ind ascid) ascid ^ ")"
  | printAcc (ACCTYP x) ind ascid =
    "ACCTYP(" ^ EL.printExtLab x (fn x => printAccId x T.printtyf' ind ascid) ascid ^ ")"
  | printAcc (ACCOVC x) ind ascid =
    "ACCOVC(" ^ EL.printExtLab x (fn x => printAccId x T.printseqty' ind ascid) ascid ^ ")"
  | printAcc (ACCSTR x) ind ascid =
    "ACCSTR(" ^ EL.printExtLab x (fn x => printAccId x (fn e => printEnv e (ind ^ tab)) ind ascid) ascid ^ ")"
  | printAcc (ACCSIG x) ind ascid =
    "ACCSIG(" ^ EL.printExtLab x (fn x => printAccId x (fn e => printEnv e (ind ^ tab)) ind ascid) ascid ^ ")"
  | printAcc (ACCFUN x) ind ascid =
    "ACCFUN(" ^ EL.printExtLab x (fn x => printAccId x (fn (e1, e2) => "(" ^ printEnv e1 (ind ^ tab) ^ ",\n" ^ printEnv e1 (ind ^ tab) ^ ")") ind ascid) ascid ^ ")"
and printocst (CSTTYP x) _ ascid =
    "  TYP(" ^ EL.printExtLab x (fn x => printPair x T.printty') ascid ^ ")"
  | printocst (CSTTYF x) _ ascid =
    "  TYF(" ^ EL.printExtLab x (fn x => printPair x T.printtyf') ascid ^ ")"
  | printocst (CSTTYN x) _ ascid =
    "  NAM(" ^ EL.printExtLab x (fn x => printPair x T.printtnty') ascid ^ ")"
  | printocst (CSTSEQ x) _ ascid =
    "  SEQ(" ^ EL.printExtLab x (fn x => printPair x T.printseqty') ascid ^ ")"
  | printocst (CSTROW x) _ ascid =
    "  REC(" ^ EL.printExtLab x (fn x => printPair x T.printrowty') ascid ^ ")"
  | printocst (CSTLAB x) _ ascid =
    "  LAB(" ^ EL.printExtLab x (fn x => printPair x T.printlabty) ascid ^ ")"
  | printocst (CSTENV x) ind ascid =
    "  ENV(" ^ EL.printExtLab x (fn x => printPair x (fn x => printEnv x (ind ^ tab))) ascid ^ ")"
  | printocst (CSTCLS x) ind ascid =
    "  CLS(" ^ EL.printExtLab x (fn x => printPair x CL.toString) ascid  ^ ")"
  | printocst (CSTSIG evsbind) ind ascid = "  SIG(" ^ printEvsBind evsbind ^ ")"
  | printocst (CSTFUN evfbind) ind ascid = "  FUN(" ^ printEvfBind evfbind ^ ")"
  | printocst (CSTSHA shabind) ind ascid = "  SHA(" ^ printShaBind shabind ^ ")"
  | printocst (CSTLET env)     ind ascid = "  LET(" ^ printEnv env (ind ^ "      ") ^ ")"
  | printocst (CSTACC acc)     ind ascid = "  ACC(" ^ printAcc acc ind ascid ^ ")"
and printocstlist []        ind1 ind2 ind3 ascid = ""
  | printocstlist [x]       ind1 ind2 ind3 ascid = ind2 ^ printocst x ind1 ascid
  | printocstlist (x :: xs) ind1 ind2 ind3 ascid = ind2 ^ printocst x ind1 ascid ^
						   "\n" ^
						   printocstlist xs ind1 (ind1 ^ ind3) ind3 ascid
and printcst' (OCST cst) ind ascid =
    OMC.foldri
	(fn (n, ocst, y) =>
	    ind ^ Int.toString n
	    ^ ":"
	    ^ printocstlist ocst ind "" (" " ^ (String.translate (fn _ => " ") (Int.toString n))) ascid
	    ^ "\n"
	    ^ y) "" cst
and printcst cst ascid = printcst' cst "" ascid

(*(*fun printIdOp NONE _ = "-"
  | printIdOp (SOME id) ascid = L.printLabst id ascid*)

(*fun printIdStOp NONE _ = "-"
  | printIdStOp (SOME (id, st)) ascid = "(" ^ L.printLabst id ascid ^ "," ^ printStatus st ^ ")"*)

fun printtvop tvop = Option.getOpt (Option.map T.printtyvar tvop, "-")

fun printcsstyl ll _ = "<" ^ L.toString ll ^ ">"

fun printcsstys (ll, s) _ = "<" ^ L.toString ll ^ "," ^ s ^ ">"

fun printcsstyz (ll, eo) ascid = "<" ^ L.toString ll         ^
				 "," ^ printIdLabOp eo ascid ^
				 ">"

(*fun printcsstya (ll, eo) ascid = "<" ^ L.toString ll        ^
				 "," ^ printIdStOp eo ascid ^
				 ">"*)

fun printcsstyt (ll, eo, (tv, l)) ascid = "<" ^ L.toString ll        ^
					  "," ^ printAsmpOp eo ascid ^
					  "," ^ "(" ^ T.printtyvar tv ^ "," ^ L.printLab l ^ ")" ^
					  ">"

fun printocss (CSM cssty) ascid = "MUL: " ^ printcsstyz cssty ascid (* MUL stands for MULti-occurrence                      *)
  | printocss (CSC cssty) ascid = "APP: " ^ printcsstyz cssty ascid (* APP stands for APPlied variable                      *)
  | printocss (CSSEXC cssty) ascid = "EXV: " ^ printcsstyz cssty ascid (* EXV stands for EXception as Variable                 *)
  | printocss (CSJ cssty) ascid = "EXD: " ^ printcsstyl cssty ascid (* EXD stands for EXception as Data. cons.              *)
  | printocss (CSI cssty) ascid = "INC: " ^ printcsstyl cssty ascid (* INC stands for INClusion                             *)
  | printocss (CSA cssty) ascid = "ANA: " ^ printcsstyl cssty ascid (* ANA stands Applied and Not Applied value             *)
  | printocss (CSF cssty) ascid = "FUN: " ^ printcsstyl cssty ascid (* FUN stands for FUN                                   *)
  | printocss (CSN cssty) ascid = "ARG: " ^ printcsstyl cssty ascid (* ARG stands for ARGument                              *)
  | printocss (CSV cssty) ascid = "FTV: " ^ printcsstyl cssty ascid (* FTV stands for Free Type Variable                    *)
  | printocss (CSP cssty) ascid = "ASP: " ^ printcsstyl cssty ascid (* ASP stands for AS in a Pattern                       *)
  | printocss (CSR cssty) ascid = "FNE: " ^ printcsstyl cssty ascid (* FNE stands for FN Expression                         *)
  | printocss (CSE cssty) ascid = "REA: " ^ printcsstyl cssty ascid (* REA stands for REAl                                  *)
  | printocss (CSD cssty) ascid = "IDE: " ^ printcsstyl cssty ascid (* IDE stands for free IDEntifier                       *)
  | printocss (CSW cssty) ascid = "PAR: " ^ printcsstys cssty ascid (* PAR stands for PARsing                               *)
  (*| printocss (CSB cssty) ascid = "BIN: " ^ printcsstyt cssty ascid (* BIN stands for BINding                               *)*)

fun printcss css ascid = foldr (fn (x, y) => (printocss x ascid) ^ "\n" ^  y) "" css

fun printcs (css, cst) ascid = printcss css ascid ^ printcst cst ascid



(*(* print the constraints into a 'dot' input *)
(* to finish *)


fun toDotOcst (CSTTYP (ty1, ty2, cds)) n ascid =
    T.printty' ty1 ^ " -- " ^ T.printty' ty2 ^
    " [ label=\"(TYP," ^ Int.toString n ^ "," ^
    CD.toStringListSt cds ascid ^ ")\" ];"
  | toDotOcst (CSTTYN (tnty1, tnty2, cds)) n ascid =
    T.printtnty' tnty1 ^ " -- " ^ T.printtnty' tnty2
    ^ "[ label=\"(NAM," ^ Int.toString n ^ "," ^
    CD.toStringListSt cds ascid ^ ")\" ];"
  | toDotOcst (CSTSEQ (seqty1, seqty2, cds)) n ascid =
    T.printseqty' seqty1 ^ " -- " ^ T.printseqty' seqty2 ^
    " [ label=\"(SEQ," ^ Int.toString n ^ "," ^
    CD.toStringListSt cds ascid ^ ")\" ];"
  | toDotOcst (CSTROW (rt1, rt2, cds)) n ascid =
    T.printrowty' rt1 ^ " -- " ^ T.printrowty' rt2 ^
    " [ label=\"REC," ^ Int.toString n ^ "," ^
    CD.toStringListSt cds ascid ^ ")\" ];"
  | toDotOcst (CSTLAB (lt1, lt2, cds)) n ascid =
    T.printlabty lt1 ^ " -- " ^ T.printlabty lt2 ^
    " [ label=\"LAB," ^ Int.toString n ^ "," ^
    CD.toStringListSt cds ascid ^ ")\" ];"
  | toDotOcst (CSTENV (ce1, ce2)) n ascid =
    "  ENV: "
    ^ "<" ^ printEnv ce1 ""
    ^ "," ^ printEnv ce2 ""
    ^ ">"
  | toDotOcst (CSTEGN (evsbind, cst)) n ascid =
    "  EGN: "
    ^ "<"  ^ printEvsBind evsbind
    ^ "\n" ^ toDotCst' cst ascid
    ^ ">"
  | toDotOcst (CSTGEN (csbind, cst1, cst2)) n ascid =
    "  GEN: "
    ^ "<"  ^ printcsbind csbind ascid
    ^ "\n" ^ toDotCst' cst1 ascid
    ^ "\n" ^ toDotCst' cst2 ascid
    ^ ">"
  | toDotOcst (CSTVAL (tvsbind1, tvsbind2, tvl, cst)) n ascid =
    "  VAL: "
    ^ "<"   ^ printTvsBind tvsbind1
    ^ ","   ^ printTvsBind tvsbind2
    ^ ","   ^ printexplexptv tvl
    ^ ",\n" ^ toDotCst' cst ascid
    ^ ">"
and toDotOcstList []        _ _     = ""
  | toDotOcstList [x]       n ascid = toDotOcst x n ascid
  | toDotOcstList (x :: xs) n ascid = toDotOcst x n ascid ^
				      "\n" ^
				      toDotOcstList xs n ascid
and toDotCst' cst ascid =
    foldricst
	(fn (n, ocst, y) =>
	    Int.toString n
	    ^ ":"
	    ^ toDotOcstList ocst n ascid
	    ^ "\n"
	    ^ y) "" cst
and toDotCst cst ascid = toDotCst' cst ascid*)



(* Accessors to a accid *)

fun getAccidI (x : accid) = #id    x
fun getAccidC (x : accid) = #class x
fun getAccidL (x : accid) = #lab   x*)



(* Bindings constructors *)

fun consBind     id bind class lab poly = EL.initExtLab (C.consBind     id bind class lab poly) lab
fun consBindPoly id bind class lab      = EL.initExtLab (C.consBindPoly id bind class lab)      lab
fun consBindMono id bind class lab      = EL.initExtLab (C.consBindMono id bind class lab)      lab

(*fun consextty    id ty   class lab poly = consBindSc   id ty           class lab poly*)
(*fun consextty'   id ty   class lab      = consBindPoly id ty           class lab*)
(*fun consexttyvar id tv   class lab      = consBindPoly id (T.consV tv) class lab*)
(*fun consexttv    id tv   class lab poly = consBindSc   id tv           class lab poly*)
(*fun consextseqty id sq         lab      = consBindPoly id sq  (CL.consOC ()) lab*)
(*fun consexv      id env  class lab      = consBindPoly id env          class lab*)
(*fun consexvar    id ev   class lab      = consBindPoly id (ENVVAR ev)    class lab*)

(*fun gettys xs =
    map (fn x => EL.mapExtLab x (fn {id, scope, bind = T.V (tv, _, _), class, lab, poly} =>
				 C.consBindSc id tv class lab poly
			       | _ => raise EH.DeadBranch ""))
	xs*)


fun consAccId lid sem class lab = {lid = lid, sem = sem, class = class, lab = lab}


(* Accessors to a extenv *)

fun getBindI x = C.getBindI (EL.getExtLabT x)
fun getBindT x = C.getBindT (EL.getExtLabT x)
fun getBindC x = C.getBindC (EL.getExtLabT x)
fun getBindL x = C.getBindL (EL.getExtLabT x)
fun getBindP x = C.getBindP (EL.getExtLabT x)

val nextenvvar     = ref 0
fun setnext     n  = nextenvvar := n
fun getenvvar   () = !nextenvvar
fun freshenvvar () = let val x = !nextenvvar in nextenvvar := x + 1; x end
fun resetEnvVar () = setnext 0

fun envvarToInt envvar = envvar

fun eqEnvvar ev1 ev2 = (ev1 = (ev2 : envvar))

fun consEnvVar ev lab = ENVVAR (ev, lab)
fun newEnvVar  lab    = consEnvVar (freshenvvar ()) lab

fun envToEnvvar (ENVVAR ev) = ev
  | envToEnvvar _ = raise EH.DeadBranch "the environment should be a variable"

fun consEnvC vids typs tyvs strs sigs funs ovcs info =
    ENVCON {vids = vids,
	    typs = typs,
	    tyvs = tyvs,
	    strs = strs,
	    sigs = sigs,
	    funs = funs,
	    ovcs = ovcs,
	    info = info}

fun consInfo lab cmp tns fct = {lab = lab, cmp = cmp, tns = tns, fct = fct}

val emgen = OME.empty
val emvar = emgen
val emtyp = emgen
val emtv  = emgen
val emstr = emgen
val emsig = emgen
val emfun = emgen
val emopn = OMO.empty
val emoc  = emgen
val emful = true        (* The environment is initially complete                       *)
val emtn  = [] : tnmap  (* The environment does not initially define any type name     *)
val emfct = false       (* The environment is initially not the parameter of a functor *)
val emnfo = consInfo L.dummyLab emful emtn emfct
val emenv = consEnvC emvar emtyp emtv emstr emsig emfun emoc emnfo


(* Accessors to environments *)

fun getVids (ENVCON x) = #vids x
  | getVids _          = raise EH.DeadBranch ""
fun getTyps (ENVCON x) = #typs x
  | getTyps _          = raise EH.DeadBranch ""
fun getTyvs (ENVCON x) = #tyvs x
  | getTyvs _          = raise EH.DeadBranch ""
fun getStrs (ENVCON x) = #strs x
  | getStrs _          = raise EH.DeadBranch ""
fun getSigs (ENVCON x) = #sigs x
  | getSigs _          = raise EH.DeadBranch ""
fun getFuns (ENVCON x) = #funs x
  | getFuns _          = raise EH.DeadBranch ""
fun getOvcs (ENVCON x) = #ovcs x
  | getOvcs _          = raise EH.DeadBranch ""
fun getInfo (ENVCON x) = #info x
  | getInfo _          = raise EH.DeadBranch ""
fun getILab (ENVCON x) = #lab (#info x)
  | getILab _          = raise EH.DeadBranch ""
fun getICmp (ENVCON x) = #cmp (#info x)
  | getICmp _          = raise EH.DeadBranch ""
fun getIFct (ENVCON x) = #fct (#info x)
  | getIFct _          = raise EH.DeadBranch ""
fun getITns (ENVCON x)        = #tns (#info x)
  | getITns (ENVSEQ (e1, e2)) = (getITns e1) @ (getITns e2)
  | getITns (ENVVAR _)        = []
  | getITns env               = (print (printEnv env ""); raise EH.DeadBranch "")

(* update fields in a record of type infoEnv *)
fun updateInfoLab lab {lab = _, cmp, tns, fct} = consInfo lab cmp tns fct
fun updateInfoCmp cmp {lab, cmp = _, tns, fct} = consInfo lab cmp tns fct
fun updateInfoTns tns {lab, cmp, tns = _, fct} = consInfo lab cmp tns fct
fun updateInfoFct fct {lab, cmp, tns, fct = _} = consInfo lab cmp tns fct

(* modifier methods to update values of constructors of the datatype env *)
fun updateVids vids (ENVCON {vids = _, typs, tyvs, strs, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs info
  | updateVids _ _ = raise EH.DeadBranch ""
fun updateTyps typs (ENVCON {vids, typs = _, tyvs, strs, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs info
  | updateTyps _ _ = raise EH.DeadBranch ""
fun updateTyvs tyvs (ENVCON {vids, typs, tyvs = _, strs, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs info
  | updateTyvs _ _ = raise EH.DeadBranch ""
fun updateStrs strs (ENVCON {vids, typs, tyvs, strs = _, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs info
  | updateStrs _ _ = raise EH.DeadBranch ""
fun updateSigs sigs (ENVCON {vids, typs, tyvs, strs, sigs = _, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs info
  | updateSigs _ _ = raise EH.DeadBranch ""
fun updateFuns funs (ENVCON {vids, typs, tyvs, strs, sigs, funs = _, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs info
  | updateFuns _ _ = raise EH.DeadBranch ""
fun updateOvcs ovcs (ENVCON {vids, typs, tyvs, strs, sigs, funs, ovcs = _, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs info
  | updateOvcs _ _ = raise EH.DeadBranch ""
fun updateILab lab (ENVCON {vids, typs, tyvs, strs, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs (updateInfoLab lab info)
  | updateILab lab (ENVSEQ (env1, env2)) = ENVSEQ (updateILab lab env1, updateILab lab env2)
  | updateILab lab (ENVLOC (env1, env2)) = ENVLOC (env1, updateILab lab env2)
  | updateILab _ env = env
fun updateICmp cmp (ENVCON {vids, typs, tyvs, strs, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs (updateInfoCmp cmp info)
  | updateICmp _ _ = raise EH.DeadBranch ""
fun updateITns tns (ENVCON {vids, typs, tyvs, strs, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs (updateInfoTns tns info)
  | updateITns _ _ = raise EH.DeadBranch ""
fun updateIFct fct (ENVCON {vids, typs, tyvs, strs, sigs, funs, ovcs, info}) =
    consEnvC vids typs tyvs strs sigs funs ovcs (updateInfoFct fct info)
  | updateIFct fct (ENVSEQ (env1, env2)) = ENVSEQ (updateIFct fct env1, updateIFct fct env2)
  | updateIFct fct (ENVLOC (env1, env2)) = ENVLOC (env1, updateIFct fct env2)
  | updateIFct _ env = env

fun projVids idG = updateVids idG emenv
fun projTyps idT = updateTyps idT emenv
fun projTyvs idV = updateTyvs idV emenv
fun projStrs idS = updateStrs idS emenv
fun projSigs idS = updateSigs idS emenv
fun projFuns idF = updateFuns idF emenv
fun projOvcs idD = updateOvcs idD emenv

fun projOpns idO = ENVOPN idO


fun isEmptyIdEnv idenv = OME.numItems idenv = 0

fun isEmptyVarEnv varenv = isEmptyIdEnv varenv
fun isEmptyTyvEnv tyvenv = isEmptyIdEnv tyvenv
fun isEmptyStrEnv strenv = isEmptyIdEnv strenv
fun isEmptyOpnEnv opnenv = OMO.isEmpty opnenv

(*fun isEmptyValEnv (env as ENVCON _) =
    isEmptyIdEnv (getVids env) andalso
    isEmptyIdEnv (getVars env) andalso
    isEmptyIdEnv (getCons env)
  | isEmptyValEnv _ = false*)

(* tests whether an environment is empty *)
fun isEmptyEnv (env as ENVCON _) =
    isEmptyIdEnv (getVids env) andalso
    isEmptyIdEnv (getTyps env) andalso
    isEmptyIdEnv (getTyvs env) andalso
    isEmptyIdEnv (getStrs env) andalso
    isEmptyIdEnv (getSigs env) andalso
    isEmptyIdEnv (getFuns env) andalso
    isEmptyIdEnv (getOvcs env)
  | isEmptyEnv (ENVDEP extenv) = isEmptyEnv (EL.getExtLabT extenv)
  | isEmptyEnv (ENVSEQ (env1, env2)) = isEmptyEnv env1 andalso isEmptyEnv env2
  | isEmptyEnv env = false

(*fun emEnvListAnd envl = foldr (fn (x, y) => isEmptyEnv x andalso y) true envl*)

fun addenv  (v, semty) env = OME.insert (env,       v, semty)
fun singenv (v, semty)     = OME.insert (OME.empty, v, semty)

fun findenv id env = OME.find (env, id)

fun remenv id env = #1 (OME.remove (env, id)) handle LibBase.NotFound => env

fun mapenv fmap env = OME.map fmap env

fun uenv envl =
    foldr (fn (env1, env2) => OME.unionWith (fn (x, y) => x @ y) (env1, env2))
	  emgen
	  envl

(*fun outenv env dom = I.foldr (fn (x, env) => remenv x env) env dom
fun inenv  env dom = I.foldr (fn (x, cenv) =>
				 case OME.find (env, x) of
				     NONE => cenv
				   | SOME y => OME.insert (cenv, x, y)) emgen dom*)
fun plusenv env1 env2 = OME.unionWith (fn (_, y) => y) (env1, env2)

fun foldrenv  ffold init env = OME.foldr  ffold init env
fun foldlenv  ffold init env = OME.foldl  ffold init env
fun foldrienv ffold init env = OME.foldri ffold init env
fun foldlienv ffold init env = OME.foldli ffold init env

fun appenv  fmap env = OME.app  fmap env
fun appienv fmap env = OME.appi fmap env

fun dom  env  = OME.foldri (fn (l, _, set) => I.add l set) I.empty env
fun doms envl = foldr (fn (x, y) => I.union (dom x) y) I.empty envl

fun plusproj env id = case findenv id env of NONE => [] | SOME x => x

(*fun updGenEnvLabs genenv labs stats cdeps =
    mapenv (fn binds => map (fn x => EL.updExtLab x labs stats cdeps) binds) genenv

(* Updating of the annotations of an environment.
 * Only used in Build.sml. *)
fun updEnvLabs (env as ENVCON _) labs stats cdeps =
    let val vids = updGenEnvLabs (getVids env) labs stats cdeps
	val vars = updGenEnvLabs (getVars env) labs stats cdeps
	val cons = updGenEnvLabs (getCons env) labs stats cdeps
	val typs = updGenEnvLabs (getTyps env) labs stats cdeps
	val tyvs = getTyvs env
	val strs = updGenEnvLabs (getStrs env) labs stats cdeps
	val sigs = updGenEnvLabs (getSigs env) labs stats cdeps
	val funs = getFuns env
	val ovcs = updGenEnvLabs (getOvcs env) labs stats cdeps
    in consEnvC vids vars cons typs tyvs strs sigs funs ovcs (getInfo env)
    end
  | updEnvLabs (env as ENVVAR _) _ _ _ = env
  | updEnvLabs (env as ENVSEQ _) _ _ _ = env
  | updEnvLabs (env as ENVOPN _) _ _ _ = env
  | updEnvLabs (env as ENVCST _) _ _ _ = env
  | updEnvLabs (INJSTR x) labs stats cdeps = INJSTR (EL.updExtLab x labs stats cdeps)
  | updEnvLabs (INJTYP x) labs stats cdeps = INJTYP (EL.updExtLab x labs stats cdeps)
  | updEnvLabs (INJVID x) labs stats cdeps = INJVID (EL.updExtLab x labs stats cdeps)


(*(**)
fun isInVidsEnv (env as ENVCON _) id = I.isin id (dom (getVids env))
  | isInVidsEnv _ _ = raise EH.DeadBranch ""
fun isInVarsEnv (env as ENVCON _) id = I.isin id (dom (getVars env))
  | isInVarsEnv _ _ = raise EH.DeadBranch ""
fun isInConsEnv (env as ENVCON _) id = I.isin id (dom (getCons env))
  | isInConsEnv _ _ = raise EH.DeadBranch ""
fun isInTypsEnv (env as ENVCON _) id = I.isin id (dom (getTyps env))
  | isInTypsEnv _ _ = raise EH.DeadBranch ""
fun isInStrsEnv (env as ENVCON _) id = I.isin id (dom (getStrs env))
  | isInStrsEnv _ _ = raise EH.DeadBranch ""*)*)


fun bindToEnv binds =
    List.foldr (fn (x, genenv) => uenv [singenv (C.getBindI (EL.getExtLabT x), [x]), genenv]) emgen binds

fun envToBind env = foldrenv (fn ([x], binds) => x :: binds
			       | _ => raise EH.DeadBranch "")
			     []
			     env

fun envsToSeq [] = emenv
  | envsToSeq [env] = env
  | envsToSeq (env :: envs) =
    if isEmptyEnv env
    then envsToSeq envs
    else ENVSEQ (env, envsToSeq envs)

val foldlOEnv = OMO.foldl
fun appOEnv fapp env = OMO.app fapp env
fun addOEnv x env = OMO.enqueue (env, x)
fun singOEnv x = addOEnv x emopn
fun uOEnv envl =
    foldl (fn (env1, env2) => OMO.foldl (fn (x, env) => addOEnv x env) env2 env1)
	  emopn
	  envl
(*fun domOEnvO env = OMO.foldl (fn ((lid, _, OST), set) => L.add (L.buildKey lid) set
			       | (_, set) => set) L.empty env
fun domOEnvR env = OMO.foldl (fn ((lid, _, DRE), set) => L.add (L.buildKey lid) set
			       | (_, set) => set) L.empty env
fun domOEnvI env = OMO.foldl (fn ((lid, _, ISI), set) => L.add (L.buildKey lid) set
			       | (_, set) => set) L.empty env

fun extractExtTy idenv =
    case (OME.listItemsi idenv) of
	[(v, [extty])] => SOME (v, extty)
      | _ => NONE

(* Union of environmnent *)
fun uenvEnvBindK DEC DEC = DEC
  | uenvEnvBindK LET LET = LET
  | uenvEnvBindK FN  FN  = FN
  | uenvEnvBindK bkd1 bkd2 = (print (printBindK bkd1 ^ " " ^ printBindK bkd2);
			      raise EH.DeadBranch "")*)

fun uenvEnvInfo {lab = lab1, cmp = cmp1, tns = tns1, fct = fct1}
		{lab = lab2, cmp = cmp2, tns = tns2, fct = fct2} =
    consInfo lab2 (*(2010-04-06)Why do we keep the second one only?*)
	     (cmp1 andalso cmp2) (* is complete if both are complete *)
	     (tns1 @ tns2)
	     (fct1 orelse fct2) (* is the argument of a functor if at least one is *)

fun uenvEnvC (ENVCON {vids = vids1, typs = typs1, tyvs = tyvs1, strs = strs1,
		      sigs = sigs1, funs = funs1, ovcs = ovcs1, info = info1})
	     (ENVCON {vids = vids2, typs = typs2, tyvs = tyvs2, strs = strs2,
		      sigs = sigs2, funs = funs2, ovcs = ovcs2, info = info2}) =
    consEnvC (uenv [vids1, vids2])
	     (uenv [typs1, typs2])
	     (uenv [tyvs1, tyvs2])
	     (uenv [strs1, strs2])
	     (uenv [sigs1, sigs2])
	     (uenv [funs1, funs2])
	     (uenv [ovcs1, ovcs2])
	     (uenvEnvInfo info1 info2)
  | uenvEnvC x y =
    if isEmptyEnv x
    then y
    else if isEmptyEnv y
    then x
    else ENVSEQ (x, y)
  (*| uenvEnvC _ _ = raise EH.DeadBranch "trying to union non mapping environments"*)

(*fun flatUEnv [] = []
  | flatUEnv ((ENVSEQ (env1, env2)) :: xs) = flatUEnv (env1 :: env2 :: xs)
  | flatUEnv (x :: xs) = x :: (flatUEnv xs)*)

fun uenvEnv [] = emenv
  | uenvEnv ((ENVSEQ (env1, env2)) :: xs) = uenvEnv (env1 :: env2 :: xs)
  | uenvEnv [x] = x
  | uenvEnv (x :: xs) = uenvEnvC x (uenvEnv xs)
  (*| uenvEnv (_ :: xs) = raise EH.DeadBranch "trying to union non mapping environments"*)


(*(* Extracts the identifiers of a structure/signature. *)
(* TODO: add something for opened structures (and functors?). *)

fun tyEnvToTyIds tyenv =
    OME.foldri
	(fn (id, semty, map) =>
	    foldl (fn ((bind, _, _, _), map) =>
		      let val cl = C.getBindC bind
		      in if CL.classIsDAT cl
			 then CL.consMap id (CL.getClassDATcons cl) map
			 else CL.consMap id (CL.CONS []) map
		      end)
		  map
		  semty)
	CL.emMap
	tyenv

(*fun strEnvToStrIds strenv =
    OME.foldri
	(fn (id, semty, map) =>
	    foldl (fn (eenv, map) =>
		      CL.consMap id (CL.getClassSTR (getBindC eenv)) map)
		  map
		  semty)
	CL.emMap
	strenv

fun sigEnvToStrIds sigenv =
    OME.foldri
	(fn (id, semty, map) =>
	    foldl (fn (eenv, map) =>
		      CL.consMap id (CL.getClassSIG (getBindC eenv)) map)
		  map
		  semty)
	CL.emMap
	sigenv*)

(*fun envToStrIds (env as ENVCON _) =
    let val va = dom            (getVids env)
	val re = dom            (getVars env)
	val co = dom            (getCons env)
	val ty = tyEnvToTyIds   (getTyps env)
	val st = strEnvToStrIds (getStrs env)
	val si = sigEnvToStrIds (getSigs env)
	val oc = dom            (getOvcs env)
	val os = L.empty
	val dr = L.empty
	val is = L.empty
	val strenv = CL.consStrEnv re co va ty st si oc os dr is
    in CL.ENV strenv
    end
  | envToStrIds (ENVOPN opnenv) =
    CL.ENV (CL.consStrEnv I.empty
			  I.empty
			  I.empty
			  CL.emMap
			  CL.emMap
			  CL.emMap
			  I.empty
			  (domOEnvO opnenv)
			  L.empty
			  L.empty)
  | envToStrIds (ENVSEQ (env1, env2)) =
    let val str1 = envToStrIds env1
	val str2 = envToStrIds env2
    in case (str1, str2) of
	   (CL.ENV strenv1, CL.ENV strenv2) => CL.ENV (CL.unionStrEnv strenv1 strenv2)
	 | _ => CL.UNB
    end
  (*(2010-04-20)WTF is that?????
   * We should have strenv's as sequences as well and the best would be to get rid of them.*)
  | envToStrIds _ = CL.UNB*)


(* transformation of environment *)

fun toPATsemty semty = map (fn x => EL.mapExtLab x (fn x => C.toPAT x)) semty
fun toRECsemty semty = map (fn x => EL.mapExtLab x (fn x => C.toREC x)) semty
fun toCONsemty semty = map (fn x => EL.mapExtLab x (fn x => C.toCON x)) semty
fun toEXCsemty semty = map (fn x => EL.mapExtLab x (fn x => C.toEXC x)) semty
fun toDATsemty semty = map (fn x => EL.mapExtLab x (fn x => C.toDAT x)) semty

fun toPAT idenv = OME.map (fn semty => toPATsemty semty) idenv
fun toREC idenv = OME.map (fn semty => toRECsemty semty) idenv
fun toCON idenv = OME.map (fn semty => toCONsemty semty) idenv
fun toEXC idenv = OME.map (fn semty => toEXCsemty semty) idenv
fun toDAT idenv = OME.map (fn semty => toDATsemty semty) idenv

fun toPATlabSemty semty labs = map (fn x => EL.mapExtLab x (fn x => C.toPATlabs x labs)) semty
fun toPATlab      idenv labs = OME.map (fn semty => toPATlabSemty semty labs) idenv

fun toVALlabSemty semty ll = map (fn x => EL.mapExtLab x (fn x => C.toVALlabs x ll)) semty
fun toVALlab idenv ll = OME.map (fn semty => toVALlabSemty semty ll) idenv

fun toCONlabSemty semty l = map (fn x => EL.mapExtLab x (fn x => C.toCONlab x l)) semty
fun toCONlab idenv l = OME.map (fn semty => toCONlabSemty semty l) idenv

fun toEXClabSemty semty l = map (fn x => EL.mapExtLab x (fn x => C.toEXClab x l)) semty
fun toEXClab idenv l = OME.map (fn semty => toEXClabSemty semty l) idenv


fun getDATconsSemtyC semty lab = map (fn (x, _, _, _) =>
					 (C.getBindI x,
					  C.getBindL x,
					  lab,
					  C.getBindT x,
					  CL.getClassVID (C.getBindC x)))
				 semty
fun getDATconsC idenv lab = OME.foldr (fn (semty, cons) => (getDATconsSemtyC semty lab) @ cons) [] idenv

(*fun getDATconsSemtyT semty = foldr (fn (x, y) => let val cl = C.getBindC x
						 in if CL.classIsDAT cl
						    then (CL.getClassDATconsC cl) @ y
						    else y
						 end) [] semty
fun getDATconsT idenv = OME.foldr (fn (semty, cons) => (getDATconsSemtyT semty) @ cons) [] idenv*)

fun toDATconsSemty semty xs = map (fn x => EL.mapExtLab x (fn x => C.toDATcons x xs)) semty
fun toDATconsC idT idC lab =
    let val cons = CL.genConsC (getDATconsC idC lab)
    in OME.map (fn semty => toDATconsSemty semty cons) idT
    end
(*fun toDATconsT idT idT' =
    let val cons = getDATconsT idT'
    in OME.map (fn semty => toDATconsSemty semty cons) idT
    end*)
fun toDATconsI idT lid lab = (*(2010-04-21)The label is not used.*)
    let val cons = CL.genConsI lid
    in OME.map (fn semty => toDATconsSemty semty cons) idT
    end


fun toREClabsSemty semty labs = map (fn x => EL.mapExtLab x (fn x => C.toREClabs x labs)) semty
fun toRECenv idenv1 idenv2 =
    OME.mapi
	(fn (id, semty) =>
	    toREClabsSemty semty (C.getlabExtChkRec (stripExtLab (plusproj idenv2 id))))
	idenv1

(*fun toSTRstrSemty semty str = map (fn x => EL.mapExtLab x (fn x => (C.toSTRstr x str))) semty
fun toSTRstr strenv str = (OME.map (fn semty => toSTRstrSemty semty str) strenv)

fun toSIGstrSemty semty str = map (fn x => EL.mapExtLab x (fn x => (C.toSIGstr x str))) semty
fun toSIGstr sigenv str = OME.map (fn semty => toSIGstrSemty semty str) sigenv*)

fun getVAL semty = List.mapPartial (fn x => if C.isVAL (EL.getExtLabT x) then SOME x else NONE) semty
fun getPAT semty = List.mapPartial (fn x => if C.isPAT (EL.getExtLabT x) then SOME x else NONE) semty
fun getCON semty = List.mapPartial (fn x => if C.isCON (EL.getExtLabT x) then SOME x else NONE) semty
fun getREC semty = List.mapPartial (fn x => if C.isREC (EL.getExtLabT x) then SOME x else NONE) semty
fun getDAT semty = List.mapPartial (fn x => if C.isDAT (EL.getExtLabT x) then SOME x else NONE) semty
fun getEXC semty = List.mapPartial (fn x => if C.isEXC (EL.getExtLabT x) then SOME x else NONE) semty

fun getVALenv idenv = OME.mapPartial (fn y => case getVAL y of [] => NONE | z => SOME z) idenv
fun getPATenv idenv = OME.mapPartial (fn y => case getPAT y of [] => NONE | z => SOME z) idenv
fun getRECenv idenv = OME.mapPartial (fn y => case getREC y of [] => NONE | z => SOME z) idenv
fun getCONenv idenv = OME.mapPartial (fn y => case getCON y of [] => NONE | z => SOME z) idenv
fun getEXCenv idenv = OME.mapPartial (fn y => case getEXC y of [] => NONE | z => SOME z) idenv
fun getDATenv idenv = OME.mapPartial (fn y => case getDAT y of [] => NONE | z => SOME z) idenv

fun getPATb semty =
    List.mapPartial (fn x => if (L.isEmpty o C.getPATval o EL.getExtLabT) x then NONE else SOME x)
		    semty

(* get the part of an environment which correspond to identifiers in pattern constrained to be
   value variables because of "as" *)
fun getPATbenv idenv = OME.mapPartial (fn y => case getPATb y of [] => NONE | z => SOME z) idenv

fun toCloseSemty  semty clos = map (fn x => EL.mapExtLab x (fn x => C.setVALclose x clos)) semty
fun toCloseVarenv idenv clos = OME.map (fn semty => toCloseSemty semty clos) idenv
fun toClose       env   clos = updateVids (toCloseVarenv (getVids env) clos) env*)


fun closeVids vids clos =
    mapenv (fn sem => map (fn x => EL.mapExtLab x (fn x => C.closeBind x clos)) sem)
	   vids


(*(* UDPATING OF STATUS OF IDS *)

(* ids is a identifier set of constructors *)
fun restrConstrExtty {id, scope, bind, class, lab, poly} ids =
    C.consBindSc id
		 bind
		 class
		 lab
		 (P.restrictPoly poly ids)

(* restrConstr restricts the poly fields in the idenv using the set
 * which is an identifier set of constructors. *)
fun restrConstr idenv ids =
    mapenv (fn semty => map (fn x => EL.mapExtLab x (fn x => restrConstrExtty x ids)) semty)
	   idenv

fun restrValueExpans (X.Expexp labs) _ = X.Expexp labs
  | restrValueExpans (X.Expdep (lid as I.ID (id, l), evop, labs, sts)) idenv =
    if I.isin id (dom idenv) andalso L.isEmpty sts
    then X.Expdep (lid, evop, labs, C.getlabExtChkRec (stripExtLab (plusproj idenv id)))
    else X.Expdep (lid, evop, labs, sts)
  | restrValueExpans (X.Expdep x) _ = X.Expdep x
(* We can't do anything if it is a long context dependency. *)

fun restrValueMono (P.EXPANS expans) idenv = P.EXPANS (restrValueExpans expans idenv)
  | restrValueMono (x as P.MONBIN _) _ = x

fun restrValuePoly P.POLY      _     = P.POLY
  | restrValuePoly (P.MONO xs) idenv = P.MONO (map (fn x => restrValueMono x idenv) xs)

fun restrValueNonexp X.Nonexp      _     = X.Nonexp
  | restrValueNonexp (X.Expans xs) idenv = X.Expans (map (fn x => restrValueExpans x idenv) xs)

(*(* idenv is an environment of value variables *)
fun restrValueExtty {id, scope, bind, class, lab, poly} idenv =
    consextty id
	      bind
	      class
	      lab
	      (restrValuePoly poly idenv)*)

(* THIS IS WHERE WE HAVE TO DO SOMETHING ELSE
 * (2010-04-06)? *)
fun restrValueBind {id, scope, bind, poly, lab, class} idenv =
    let val ls = C.getlabExtChkRec (stripExtLab (plusproj idenv id))
	val cl = if CL.classIsVAL class andalso not (L.isEmpty ls)
		 then CL.classToREClabs class lab ls
		 else class
    in C.consBind id
		  scope
		  bind
		  cl
		  lab
		  (restrValuePoly poly idenv)
    end

fun restrValueBindList xs idenv = map (fn x => EL.mapExtLab x (fn x => restrValueBind x idenv)) xs

(* idenv is an environment of value variables *)
fun restrValue xs idenv = mapenv (fn semty => restrValueBindList semty idenv) xs

fun updateStatus idenv domC idenvX = restrValue (restrConstr idenv domC) idenvX

(* recompute the new idG4 and idX4 from env2 depending on the statuses in env1 *)
fun updateEnvStatus env1 env2 =
    let val domC3 = dom (getCons env1)
	val domX3 = dom (getVars env1)
	val idG4' = updateStatus
			(outenv (getVids env2) (I.union domX3 domC3))
			domC3
			(getVars env1)
	val idXt  = toRECenv (inenv (getVids env2) domX3) (getVars env1)
	val idX4' = updateStatus
			(uenv [getVars env2, idXt])
			domC3
			(getVars env1)
    in (idG4', idX4')
    end*)

fun plusEnv (env1 as ENVCON _) (env2 as ENVCON _) =
    let val vids = plusenv (getVids env1) (getVids env2)
	val typs = plusenv (getTyps env1) (getTyps env2)
	val tyvs = plusenv (getTyvs env1) (getTyvs env2)
	val strs = plusenv (getStrs env1) (getStrs env2)
	val sigs = plusenv (getSigs env1) (getSigs env2)
	val funs = plusenv (getFuns env1) (getFuns env2)
	val ovcs = plusenv (getOvcs env1) (getOvcs env2)
	val info = uenvEnvInfo (getInfo env1) (getInfo env2)
	val env = consEnvC vids typs tyvs strs sigs funs ovcs info
    in env
    end
  | plusEnv (ENVSEQ (env1, env2)) env3 = ENVSEQ (env1, plusEnv env2 env3)
  | plusEnv env1 (ENVSEQ (env2, env3)) = ENVSEQ (plusEnv env1 env2, env3)
  | plusEnv env1 env2 =
    if isEmptyEnv env1
    then env2
    else if isEmptyEnv env2
    then env1
    else ENVSEQ (env1, env2)


(*fun dumPush x labs stts deps = x*)

fun pushExtIdEnv idenv labs stts deps(* f*) =
    mapenv (fn sem => map (fn bind => EL.updExtLab bind(* (EL.mapExtLab bind (fn bind => C.mapBind bind (fn x => f x labs stts deps)))*)
						labs
						stts
						deps)
			  sem)
	   idenv

fun pushExtEnv (env as ENVCON _) labs stts deps =
    if isEmptyEnv env
    then ENVDEP (env, labs, stts, deps)
    else let val vids = pushExtIdEnv (getVids env) labs stts deps(* dumPush*)
	     val typs = pushExtIdEnv (getTyps env) labs stts deps(* dumPush*)
	     val tyvs = pushExtIdEnv (getTyvs env) labs stts deps(* dumPush*)
	     val strs = pushExtIdEnv (getStrs env) labs stts deps(* pushExtEnv*)
	     val sigs = pushExtIdEnv (getSigs env) labs stts deps(* pushExtEnv*)
	     val funs = pushExtIdEnv (getFuns env) labs stts deps(* pushExtFunEnv*)
	     val ovcs = pushExtIdEnv (getOvcs env) labs stts deps(* dumPush*)
	 in consEnvC vids typs tyvs strs sigs funs ovcs (getInfo env)
	 end
  | pushExtEnv (ENVSEQ (env1, env2)) labs stts deps =
    ENVSEQ (pushExtEnv env1 labs stts deps, pushExtEnv env2 labs stts deps)
  (*(2010-06-09)NOTE: we shouldn't need to push onto env1 because this environment
   * should be useless. *)
  | pushExtEnv (ENVDEP (env, labs0, stts0, deps0)) labs stts deps =
    pushExtEnv env (L.union labs0 labs) (L.union stts0 stts) (CD.union deps0 deps)
  | pushExtEnv env labs stts deps = ENVDEP (env, labs, stts, deps)

(*and pushExtFunEnv (env1, env2) labs stts deps =
    (pushExtEnv env1 labs stts deps,
     pushExtEnv env2 labs stts deps)*)


(*
fun extractExtEnv strenv = case (OME.listItemsi strenv) of
			       [(v, [STV extenv])] => SOME (v, extenv)
			     |  _                  => NONE
*)



(* constraints *)


(*fun conscsss css cst = (css, cst)*)
(*fun getcsSyn (css, _) = css*)
(*fun getcsSem (_, cst) = cst*)

val emcss = []
val emcst = OCST OMC.empty

(*val emcs  = (emcss, emcst)*)

(*fun getcsSemi cs i = case OMC.find (getcsSem cs, i) of NONE => [] | SOME x => x*)
fun getcstSemi cst i = case OMC.find (cst, i) of NONE => [] | SOME x => x

(*fun replacecsSyn css cs = conscsss css (getcsSem cs)
fun replacecsSem cst cs = conscsss (getcsSyn cs) cst
fun updatecsSyn css cs = conscsss css (getcsSem cs)
fun updatecsSem cst cs = conscsss (getcsSyn cs) cst
fun updatecstSem (v, cl) (OCST cst) = OCST (OMC.insert (cst, v, cl))

fun projcsSyn css = (css, emcst)
fun projcsSem cst = (emcss, cst)*)

fun conscss c css = c :: css
fun conscst (v, c) (OCST cst) = OCST (OMC.insert (cst, L.toInt v, c :: (getcstSemi cst (L.toInt v))))
fun conscsss cl css = cl @ css
fun conscsts (v, cs) (OCST cst) = OCST (OMC.insert (cst, L.toInt v, cs @ (getcstSemi cst (L.toInt v))))

(*fun conscsSyn c cs = replacecsSyn (conscssSyn c (getcsSyn cs)) cs
fun conscsSem (v, c) cs = replacecsSem (conscstSem (v, c) (getcsSem cs)) cs
fun conscsSyns cl cs = replacecsSyn (conscssSyns cl (getcsSyn cs)) cs
fun conscsSems (v, cl) cs = replacecsSem (conscstSems (v, cl) (getcsSem cs)) cs*)

fun singcss  c  = [c]
fun singcsss cs = cs
fun singcst  (v, c)  = conscst  (v, c)  emcst
fun singcsts (v, cs) = conscsts (v, cs) emcst

(*fun singcsSyn c = projcsSyn (singcssSyn c)
fun singcsSem (v, c) = projcsSem (singcstSem (v, c))
fun singcsSyns cl = projcsSyn (singcssSyns cl)
fun singcsSems (v, cl) = projcsSem (singcstSems (v, cl))*)

fun uenv2css css1 css2 = css1 @ css2
fun uenvcss xs = foldr (fn (x, y) => uenv2css x y) emcss xs
fun uenv2cst (OCST cst1) (OCST cst2) = OCST (OMC.unionWith (fn (x, y) => x @ y) (cst1, cst2))
fun uenvcst xs = foldr (fn (x, y) => uenv2cst x y) emcst xs

(*fun uenvcs cs1 cs2 = (uenvcsSyn (getcsSyn cs1) (getcsSyn cs2), uenv2cst (getcsSem cs1) (getcsSem cs2))
fun uenvcss xs = foldr (fn (x, y) => uenvcs x y) emcs xs*)

(*fun concatcsSyn css cs = replacecsSyn (uenv2css css (getcsSyn cs)) cs
fun concatcsSem cst cs = replacecsSem (uenv2cst cst (getcsSem cs)) cs*)

fun foldlicst ffold init (OCST cst) = OMC.foldli ffold init cst
fun foldricst ffold init (OCST cst) = OMC.foldri ffold init cst
fun mapicst   ffold      (OCST cst) = OCST (OMC.mapi ffold cst)

(*fun getnbcst' cst = foldlicst
			(fn (_, x, y) =>
			    (foldr
				 (fn (CSTGEN (_, cst1, cst2), z) =>
				     (getnbcst' cst1) + (getnbcst' cst2) + z + 1
				   | (CSTVAL (_, _, _, cst), z) =>
				     (getnbcst' cst) + z + 1
				   (*| (CSTBIND (_, cst1, cst2), z) =>
				     (getnbcst' cst1) + (getnbcst' cst2) + z + 1*)
				   | (_, z) => z + 1)
				 0
				 x) + y)
			0
			cst
and getnbcst    cs = getnbcst' (getcsSem cs)
fun getnbcsttop cs = foldlicst (fn (_, x, y) => (List.length x) + y) 0 (getcsSem cs)
fun getnbcss    cs = List.length (getcsSyn cs)
fun getnbcs     cs = (getnbcss cs) + (getnbcst cs)*)

fun getnbcst' cst =
    foldlicst (fn (_, cs, nb) =>
		  foldl (fn (CSTTYP _, nb) => 1 + nb
			  | (CSTTYN _, nb) => 1 + nb
			  | (CSTSEQ _, nb) => 1 + nb
			  | (CSTROW _, nb) => 1 + nb
			  | (CSTLAB _, nb) => 1 + nb
			  | (CSTCLS _, nb) => 1 + nb
			  | (CSTTYF _, nb) => 1 + nb
			  | (CSTACC _, nb) => 1 + nb
			  | (CSTSIG _, nb) => 1 + nb
			  | (CSTFUN _, nb) => 1 + nb
			  | (CSTSHA _, nb) => 1 + nb
			  | (CSTENV ((env1, env2), _, _, _), nb) => (getnbcstenv env1) + (getnbcstenv env2) + nb
			  | (CSTLET env, nb) => (getnbcstenv env) + nb)
			nb
			cs)
	      0
	      cst

and getnbcstenv (ENVCON _) = 0
  | getnbcstenv (ENVVAR _) = 0
  | getnbcstenv (ENVSEQ (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVLOC (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVSHA (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVSIG (env1, env2, _)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVWHR (env, _)) = getnbcstenv env
  | getnbcstenv (ENVPOL (_, env)) = getnbcstenv env
  | getnbcstenv (ENVDAT (_, env)) = getnbcstenv env
  | getnbcstenv (ENVOPN _) = 0
  | getnbcstenv (ENVDEP eenv) = getnbcstenv (EL.getExtLabT eenv)
  | getnbcstenv (ENVFUN cst)  = getnbcst' cst
  | getnbcstenv (ENVCST cst)  = getnbcst' cst
  | getnbcstenv (ENVPTY _) = 0
  | getnbcstenv (ENVFIL (file, env, strm)) = (getnbcstenv env) + (getnbcstenv (strm ()))
  | getnbcstenv ENVTOP = 0

fun getnbcst (env, css) = getnbcstenv env

fun getnbcsttop (env, css) = getnbcstenv env (* NOTE: this not useful anymore *)

fun getnbcss' css = List.length css

fun getnbcss (envc, css) = getnbcss' css

fun getnbcs (env, css) = (getnbcstenv env) + (getnbcss' css)

(*fun csenvdom xs = foldr (fn ((csb, _, _, _), y) => I.add (C.getBindI csb) y) I.empty xs

(*fun isEmptyCsBind (csenv1, csenv2, tcsenv, strcsenv, sigcsenv, clenv, opnbind, bk) =
    List.null csenv1   andalso
    List.null csenv2   andalso
    List.null tcsenv   andalso
    List.null strcsenv andalso
    List.null sigcsenv andalso
    List.null clenv    andalso
    isEmptyOpnEnv opnbind*)

(*fun isEmptyCsBind csbind = isEmptyEnv csbind*)

(*fun consIdToBind {id, scope, bind, class, lab, poly} xs = C.consBind id xs bind class lab poly
fun extenvToBind consid xs = consIdToBind consid xs*)

fun nestcsOneSem v ocstl (cst as (OCST mcst)) =
    case OMC.find (mcst, v) of
	NONE =>
	foldricst
	    (fn (k, ocstl', (cst', false)) =>
		let
		    val (ocstl'', b) =
			foldr
			    (fn (CSTGEN (x, cst1, cst2), (ocstl'', false)) =>
				let
				    val (cst1', b) = nestcsOneSem v ocstl cst1
				    val (cst2', b) = if b
						     then (cst2, b)
						     else nestcsOneSem v ocstl cst2
				(*val _ = Debug.printdebug2 ("-" ^ (Bool.toString b) ^ "\n")*)
				in ((CSTGEN (x, cst1', cst2')) :: ocstl'', b)
				end
			      | (CSTVAL (x, y, z, cst), (ocstl'', false)) =>
				let
				    val (cst', b) = nestcsOneSem v ocstl cst
				in ((CSTVAL (x, y, z, cst')) :: ocstl'', b)
				end
			      | (CSTEGN (x, cst), (ocstl'', false)) =>
				let
				    val (cst', b) = nestcsOneSem v ocstl cst
				in ((CSTEGN (x, cst')) :: ocstl'', b)
				end
			      (*| (CSTBIND (x, cst1, cst2), (ocstl'', false)) =>
				let
				    val (cst1', b) = nestcsOneSem v ocstl cst1
				    val (cst2', b) = if b
						     then (cst2, b)
						     else nestcsOneSem v ocstl cst2
				in ((CSTBIND (x, cst1', cst2')) :: ocstl'', b)
				end*)
			      | (x, (ocstl'', b)) => (x :: ocstl'', b))
			    ([], false)
			    ocstl'
		in (conscstSems (k, ocstl'') cst', b)
		end
	      | (k, ocstl', (cst', true)) =>
		(conscstSems (k, ocstl') cst', true))
	    (emcst, false)
	    cst
      | _ => (conscstSems (v, ocstl) cst, true)

fun nestcstSem cst cst' =
    foldricst (fn (k, ocstl, cst'') => #1 (nestcsOneSem k ocstl cst'')) cst' cst

fun nestcsSem cst (css', cst') = (css', nestcstSem cst cst')


(* extracts the constraints associated to a label*)


fun getConsLab (cst as (OCST mcst)) lab =
    case OMC.find (mcst, lab) of
	NONE =>
	foldricst
	    (fn (_, ocstl, xs) =>
		if List.null xs
		then foldr
			 (fn (CSTGEN (_, cst1, cst2), xs) =>
			     if List.null xs
			     then
				 let
				     val xs = getConsLab cst1 lab
				 in if List.null xs
				    then getConsLab cst2 lab
				    else xs
				 end
			     else xs
			   | (CSTVAL (_, _, _, cst), xs) =>
			     if List.null xs
			     then getConsLab cst lab
			     else xs
			   | (CSTEGN (_, cst), xs) =>
			     if List.null xs
			     then getConsLab cst lab
			     else xs
			   (*| (CSTBIND (_, cst1, cst2), xs) =>
			     if List.null xs
			     then
				 let
				     val xs = getConsLab cst1 lab
				 in if List.null xs
				    then getConsLab cst2 lab
				    else xs
				 end
			     else xs*)
			   | (_, xs) => xs)
			 []
			 ocstl
		else xs)
	    []
	    cst
      | (SOME xs) => xs


(* returns the environment variable associated to the signature of a resulting structure *)


(*fun getSigOfStr cst ev =
    foldricst
	(fn (_, ocstl, evop) =>
	    if Option.isSome evop
	    then evop
	    else foldr
		     (fn (CSTGEN (_, cst1, cst2), evop) =>
			 if Option.isSome evop
			 then evop
			 else
			     let
				 val evop = getSigOfStr cst1 ev
			     in if Option.isSome evop
				then evop
				else getSigOfStr cst2 ev
			     end
		       | (CSTVAL (_, _, _, cst), evop) =>
			 if Option.isSome evop
			 then evop
			 else getSigOfStr cst ev
		       | (CSTEGN ((ev1, _, _, evop1, _), cst), evop) =>
			 if Option.isSome evop
			 then evop
			 else (case evop1 of
			      NONE => getSigOfStr cst ev
			    | SOME ev2 => if ev = ev2
					  then SOME ev1
					  else getSigOfStr cst ev)
		       | (_, evop) => evop)
		     NONE
		     ocstl)
	NONE
	cst
*)

fun getSigOfStr cst ev =
    foldricst
	(fn (k, ocstl, (cst, evop)) =>
	    if Option.isSome evop
	    then (conscstSems (k, ocstl) cst, evop)
	    else
		let
		    val (ocstl, evop) =
			foldr
			    (fn (x as (CSTGEN (y, cst1, cst2)), (ocstl, evop)) =>
				if Option.isSome evop
				then (x :: ocstl, evop)
				else
				    let
					val (cst, evop) = getSigOfStr cst1 ev
				    in if Option.isSome evop
				       then ((CSTGEN (y, cst, cst2)) :: ocstl, evop)
				       else
					   let
					       val (cst', evop) = getSigOfStr cst2 ev
					   in ((CSTGEN (y, cst, cst')) :: ocstl, evop)
					   end
				    end
			      | (x as (CSTVAL (u, v, w, cst)), (ocstl, evop)) =>
				if Option.isSome evop
				then (x :: ocstl, evop)
				else
				    let
					val (cst, evop) = getSigOfStr cst ev
				    in ((CSTVAL (u, v, w, cst)) :: ocstl, evop)
				    end
			      | (x as (CSTEGN (y as (ev1, _, _, evop2, _, _), cst)), (ocstl, evop)) =>
				if Option.isSome evop
				then (x :: ocstl, evop)
				else (case evop2 of
					  NONE =>
					  let
					      val (cst, evop) = getSigOfStr cst ev
					  in ((CSTEGN (y, cst)) :: ocstl, evop)
					  end
					| SOME ev' =>
					  if ev = ev' (* then we remove the constraint and save it for later *)
					  then (ocstl, SOME (ev1, cst))
					  else
					      let
						  val (cst, evop) = getSigOfStr cst ev
					      in ((CSTEGN (y, cst)) :: ocstl, evop)
					      end)
			      (*| (x as (CSTBIND (y, cst1, cst2)), (ocstl, evop)) =>
				if Option.isSome evop
				then (x :: ocstl, evop)
				else
				    let
					val (cst, evop) = getSigOfStr cst1 ev
				    in if Option.isSome evop
				       then ((CSTBIND (y, cst, cst2)) :: ocstl, evop)
				       else
					   let
					       val (cst', evop) = getSigOfStr cst2 ev
					   in ((CSTBIND (y, cst, cst')) :: ocstl, evop)
					   end
				    end*)
			      | (_, u) => u)
			    ([], NONE)
			    ocstl
		in (conscstSems (k, ocstl) cst, evop)
		end)
	(emcst, NONE)
	cst


(* returns the environment constrained to be equal to the environment variable *)


(* It will run forever if there is a circularity problem. *)
fun getEnvEqVar cst ev =
    foldricst
	(fn (_, ocstl, envop) =>
	    if Option.isSome envop
	    then envop
	    else foldr (fn (CSTGEN (_, cst1, cst2), envop) =>
			   if Option.isSome envop
			   then envop
			   else let val envop = getEnvEqVar cst1 ev
				in if Option.isSome envop
				   then envop
				   else getEnvEqVar cst2 ev
				end
			 | (CSTVAL (_, _, _, cst), envop) =>
			   if Option.isSome envop
			   then envop
			   else getEnvEqVar cst ev
			 | (CSTEGN (_, cst), envop) =>
			   if Option.isSome envop
			   then envop
			   else getEnvEqVar cst ev
			 | (CSTENV ((ENVVAR ev', env as (ENVCON _)), _, _, _), envop) =>
			   if Option.isSome envop
			   then envop
			   else if ev = ev'
			   then SOME env
			   else envop
			 | (CSTENV ((ENVVAR ev', ENVVAR ev''), _, _, _), envop) =>
			   if Option.isSome envop
			   then envop
			   else if ev = ev'
			   then getEnvEqVar cst ev''
			   else envop
			 (*| (CSTBIND (_, cst1, cst2), envop) =>
			  if Option.isSome envop
			  then envop
			  else
			      let
				  val envop = getEnvEqVar cst1 ev
			      in if Option.isSome envop
				 then envop
				 else getEnvEqVar cst2 ev
			      end*)
			 | (_, envop) => envop)
		       NONE
		       ocstl)
	NONE
	cst*)


(* we get the labels at the csbindings *)

fun getlabsidenv idenv =
    OME.foldr
	(fn (sem, labs) => L.union (L.ord (map (fn (x, _, _, _) => C.getBindL x) sem)) labs)
	L.empty
	idenv

fun getlabopnenv opnenv =
    OMO.foldl (fn ((_, lab, _), set) => L.cons lab set) L.empty opnenv

fun combineLabsEnv (outlabs1, inlabs1) (outlabs2, inlabs2) =
    (outlabs1 @ outlabs2, L.union inlabs1 inlabs2)

fun getlabsenv (env as ENVCON _) =
    let val labsVids = getlabsidenv (getVids env)
	val labsTyps = getlabsidenv (getTyps env)
	val labsTyvs = getlabsidenv (getTyvs env)
	val labsStrs = getlabsidenv (getStrs env)
	val labsSigs = getlabsidenv (getSigs env)
	val labsFuns = getlabsidenv (getFuns env)
	val labsOvcs = getlabsidenv (getOvcs env)
	(*(2010-04-06)Functors and type variables ?*)
    in ([L.unions [labsVids, labsTyps, labsTyvs, labsStrs, labsSigs, labsFuns, labsOvcs]], L.empty)
    end
  | getlabsenv (ENVSEQ (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVLOC (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVSHA (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVSIG (env1, env2, _)) = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVWHR (env, _))        = getlabsenv env (*Don't we want to get the label of the longtyp?*)
  | getlabsenv (ENVPOL (tyvenv, env))   = combineLabsEnv ([getlabsidenv tyvenv], L.empty) (getlabsenv env)
  | getlabsenv (ENVDAT (idl, env))      = getlabsenv env
  | getlabsenv (ENVOPN opnenv)          = ([getlabopnenv opnenv], L.empty)
  | getlabsenv (ENVDEP eenv)            = getlabsenv (EL.getExtLabT eenv)
  | getlabsenv (ENVFUN cst)             = getlabscst cst
  | getlabsenv (ENVCST cst)             = getlabscst cst
  | getlabsenv (ENVVAR _)               = ([], L.empty)
  | getlabsenv (ENVPTY _)               = ([], L.empty)
  | getlabsenv (ENVFIL (f, env, strm))  = combineLabsEnv (getlabsenv env) (getlabsenv (strm ()))
  | getlabsenv ENVTOP                   = ([], L.empty)
and getlabcsbindocst (CSTTYP _)   = ([], L.empty)
  | getlabcsbindocst (CSTTYF _)   = ([], L.empty)
  | getlabcsbindocst (CSTTYN _)   = ([], L.empty)
  | getlabcsbindocst (CSTSEQ _)   = ([], L.empty)
  | getlabcsbindocst (CSTROW _)   = ([], L.empty)
  | getlabcsbindocst (CSTLAB _)   = ([], L.empty)
  | getlabcsbindocst (CSTENV _)   = ([], L.empty)
  | getlabcsbindocst (CSTCLS _)   = ([], L.empty)
  | getlabcsbindocst (CSTACC _)   = ([], L.empty)
  | getlabcsbindocst (CSTLET env) = getlabsenv env
  | getlabcsbindocst (CSTSIG _)   = ([], L.empty)
  | getlabcsbindocst (CSTFUN _)   = ([], L.empty)
  | getlabcsbindocst (CSTSHA _)   = ([], L.empty)
and getlabscst cst =
    foldricst
	(fn (_, ocstl, labsenv) =>
	    foldr (fn (ocst, labsenv) => combineLabsEnv (getlabcsbindocst ocst) labsenv)
		  labsenv
		  ocstl)
	([], L.empty)
	cst
and getbindings env = getlabsenv env


(**)

(*fun genIdCst lid cl l = {id = lid, class = cl, lab = l}*)


(* ====== Generation of type constraints ====== *)


fun genCstAllGen x1 x2 labs sts cds = EL.consExtLab (x1, x2) labs sts cds

fun genCstTyAll x1 x2 labs sts cds = CSTTYP (genCstAllGen x1 x2 labs sts cds)
fun genCstTfAll x1 x2 labs sts cds = CSTTYF (genCstAllGen x1 x2 labs sts cds)
fun genCstTnAll x1 x2 labs sts cds = CSTTYN (genCstAllGen x1 x2 labs sts cds)
fun genCstSqAll x1 x2 labs sts cds = CSTSEQ (genCstAllGen x1 x2 labs sts cds)
fun genCstRtAll x1 x2 labs sts cds = CSTROW (genCstAllGen x1 x2 labs sts cds)
fun genCstLtAll x1 x2 labs sts cds = CSTLAB (genCstAllGen x1 x2 labs sts cds)
fun genCstEvAll x1 x2 labs sts cds = CSTENV (genCstAllGen x1 x2 labs sts cds)
fun genCstClAll x1 x2 labs sts cds = CSTCLS (genCstAllGen x1 x2 labs sts cds)

fun genAccAllGen x labs sts cds = EL.consExtLab x labs sts cds

fun genAccIvAll x labs sts cds = ACCVAR (genAccAllGen x labs sts cds)
fun genAccIeAll x labs sts cds = ACCETV (genAccAllGen x labs sts cds)
fun genAccItAll x labs sts cds = ACCTYP (genAccAllGen x labs sts cds)
fun genAccIoAll x labs sts cds = ACCOVC (genAccAllGen x labs sts cds)
fun genAccIsAll x labs sts cds = ACCSTR (genAccAllGen x labs sts cds)
fun genAccIiAll x labs sts cds = ACCSIG (genAccAllGen x labs sts cds)
fun genAccIfAll x labs sts cds = ACCFUN (genAccAllGen x labs sts cds)


fun genCstGen x1 x2 lab cds = EL.consExtLab (x1, x2) (L.singleton lab) L.empty cds

fun genCstTy x1 x2 lab cds = CSTTYP (genCstGen x1 x2 lab cds)
fun genCstTf x1 x2 lab cds = CSTTYF (genCstGen x1 x2 lab cds)
fun genCstTn x1 x2 lab cds = CSTTYN (genCstGen x1 x2 lab cds)
fun genCstSq x1 x2 lab cds = CSTSEQ (genCstGen x1 x2 lab cds)
fun genCstRt x1 x2 lab cds = CSTROW (genCstGen x1 x2 lab cds)
fun genCstLt x1 x2 lab cds = CSTLAB (genCstGen x1 x2 lab cds)
fun genCstEv x1 x2 lab cds = CSTENV (genCstGen x1 x2 lab cds)
fun genCstCl x1 x2 lab cds = CSTCLS (genCstGen x1 x2 lab cds)

fun genAccGen x lab cds = EL.consExtLab x (L.singleton lab) L.empty cds

fun genAccIv x lab cds = ACCVAR (genAccGen x lab cds)
fun genAccIe x lab cds = ACCETV (genAccGen x lab cds)
fun genAccIt x lab cds = ACCTYP (genAccGen x lab cds)
fun genAccIo x lab cds = ACCOVC (genAccGen x lab cds)
fun genAccIs x lab cds = ACCSTR (genAccGen x lab cds)
fun genAccIi x lab cds = ACCSIG (genAccGen x lab cds)
fun genAccIf x lab cds = ACCFUN (genAccGen x lab cds)


(* x1 and x2 here are eg T.NC (tn1, b1,l1) and T.NC(tn2,b2,l2) *)
fun genCstEmGen x1 x2 lab = EL.initExtLab (x1, x2) lab


fun genCstTyEm x1 x2 lab = CSTTYP (genCstEmGen x1 x2 lab)
fun genCstTfEm x1 x2 lab = CSTTYF (genCstEmGen x1 x2 lab)
fun genCstTnEm x1 x2 lab = CSTTYN (genCstEmGen x1 x2 lab) (* change here for eqtypes? genCstEmGen takes and returns now 4-tuple?*)
fun genCstSqEm x1 x2 lab = CSTSEQ (genCstEmGen x1 x2 lab)
fun genCstRtEm x1 x2 lab = CSTROW (genCstEmGen x1 x2 lab)
fun genCstLtEm x1 x2 lab = CSTLAB (genCstEmGen x1 x2 lab)
fun genCstEvEm x1 x2 lab = CSTENV (genCstEmGen x1 x2 lab)
fun genCstClEm x1 x2 lab = CSTCLS (genCstEmGen x1 x2 lab)

fun genAccEmGen x lab = EL.initExtLab x lab

fun genAccIvEm x lab = ACCVAR (genAccEmGen x lab)
fun genAccIeEm x lab = ACCETV (genAccEmGen x lab)
fun genAccItEm x lab = ACCTYP (genAccEmGen x lab)
fun genAccIoEm x lab = ACCOVC (genAccEmGen x lab)
fun genAccIsEm x lab = ACCSTR (genAccEmGen x lab)
fun genAccIiEm x lab = ACCSIG (genAccEmGen x lab)
fun genAccIfEm x lab = ACCFUN (genAccEmGen x lab)


fun isMonoBind bind = P.isMono (getBindP bind)

fun toMonoVids vids labs =
    mapenv (fn sems => map (fn extlab => EL.mapExtLab extlab (fn x => C.toMonoBind x (L.cons (getBindL extlab) labs))) sems)
	   vids

fun toPolyVids vids =
    mapenv (fn sems => map (fn extlab => EL.mapExtLab extlab (fn x => C.toPolyBind x)) sems)
	   vids

fun toRECVids vids labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toREC) labs L.empty CD.empty) sems)
	   vids

fun toPATVids vids labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toPAT) labs L.empty CD.empty) sems)
	   vids

fun toEX0Vids vids labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toEX0) labs L.empty CD.empty) sems)
	   vids

fun toEX1Vids vids labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toEX1) labs L.empty CD.empty) sems)
	   vids

fun toDA0Vids vids labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toDA0) labs L.empty CD.empty) sems)
	   vids

fun toDA1Vids vids labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toDA1) labs L.empty CD.empty) sems)
	   vids

fun toDATVids vids labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toDAT) labs L.empty CD.empty) sems)
	   vids

fun toCLSVids vids cls labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x (fn x => C.toCLS x cls)) labs L.empty CD.empty) sems)
	   vids

(* We have DAT here because this is only used for datatypes and datatype descriptions. *)
fun toTYCONTyps typs cons b labs =
    let fun mapbind x = C.mapBind x (fn (tyf, _, _) => (tyf, DAT, ref (cons, b)))
    in mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x mapbind) labs L.empty CD.empty) sems)
	      typs
    end

fun allEqualVids vids =
    let val ty = T.newV ()
    in foldrenv (fn (sems, cst) =>
		    foldr (fn (bind, cst) =>
			      let val ty' = getBindT bind
				  val lab = getBindL bind
				  val c   = genCstTyEm ty ty' lab
			      in conscst (lab, c) cst
			      end)
			  cst
			  sems)
		emcst
		vids
    end



(*(*(* flatten an environment *)


fun flattenIdEnv idenv =
    foldrienv
	(fn (id, (semty, opn), (idenv, cst)) =>
	    let
		val (semty, cst) =
		    foldr
			(fn ({ty, chk, asmp, lab}, (semty, cst)) =>
			    let
				val tv = T.freshtyvar ()
				val c  = CSTTYP (T.V tv, ty, L.empty)
			    in ((T.consextty (T.V tv) chk asmp lab) :: semty,
				conscstSem (lab, c) cst)
			    end)
			([], cst)
			semty
	    in (addenv (id, (semty, opn)) idenv, cst)
	    end)
	(emid, emcst)
	idenv

fun flattenStrEnv (ENVSEQ strenv) =
    foldrienv
	(fn (id, (semty, opn), (ENVSEQ strenv, cst)) =>
	    let
		val (semty, cst) =
		    foldr
			(fn (EENV (env, lab), (semty, cst)) =>
			    let
				val ev = freshenvvar ()
				val c  = CSTENV (ENVVAR ev, env)
			    in ((consexv (ENVVAR ev) lab) :: semty,
				conscstSem (lab, c) cst)
			    end)
			([], cst)
			semty
	    in (ENVSEQ (addenv (id, (semty, opn)) strenv), cst)
	    end)
	(emstr, emcst)
	strenv

fun flattenEnv (ENVCON ((idG, idX, idC, idT, idV, idS, idO, idD), l, n)) =
    let
	val (idG', csG) = flattenIdEnv  idG
	val (idX', csX) = flattenIdEnv  idX
	val (idC', csC) = flattenIdEnv  idC
	val (idT', csT) = flattenIdEnv  idT
	val (idS', csS) = flattenStrEnv idS
    in (ENVCON ((idG', idX', idC', idT', idV, idS', idO), l, n),
	uenvcsSems [csG, csX, csC, csT, csS])
    end
  | flattenEnv (x as (ENVVAR _)) = (x, emcst)
  | flattenEnv (x as (INJSTR _)) = (x, emcst)
  | flattenEnv (x as (INJVID _)) = (x, emcst)
  | flattenEnv (x as (INJTYP _)) = (x, emcst)*)


(* Do we have to go inside INJSTR and INJVID as well? *)
(* We either return the environment in which we might find the last id
 * (with true -> NONE as the rest of the long id)
 * or we return the environment in whih we can't go down because
 * either it it a variable or because the id we're looking for is not in. *)
fun followPath' (I.ID (id, l)) x = (x, id, l, NONE)
  | followPath' (I.LID ((id, l), lid, lab)) (env as ENVCON _, labs, stts, deps) =
    let	val semty = plusproj (getStrs env) id
	val etvop = List.find (fn x => true (*(getextenvL x) = lab*)) semty
    in case etvop of
	   NONE =>
	   (*(2010-01-27)We shouldn't have lab here.*)
	   (*(2010-02-24)If we failed to find the id in the structure then
	    * there is an unmatched error that we want to report, so we return
	    * the environment in which we failed to find id and its label,
	    * the rest of the identifier that we failed to look up and the
	    * structure of the environment.*)
	   ((env, L.cons l (L.cons lab labs), stts, deps), id, l, SOME lid)
	 | SOME (x as (bind, labs', stts', deps')) =>
	   followPath' lid
		       (C.getBindT bind,
			L.cons l (L.cons lab (L.concat labs labs')),
			L.concat stts stts',
			CD.union deps deps')
    end
  | followPath' (I.LID ((id, l), lid, _)) x =
    (*(2010-02-24)If the env is not a constructor then we return the
     * env, the id that we failed to look up, the rest of the lid that
     * we haven't search yet and the supposed structure of the env.*)
    (x, id, l, SOME lid)

fun followPath lid env = followPath' lid (env, L.empty, L.empty, CD.empty)

fun followPathStr lid env = (* env has to be a ENVCON *)
    case followPath lid env of
	((env as ENVCON _, _, _, _), id, _, NONE) =>
	let val semty = plusproj (getStrs env) id
	    val etvop = List.find (fn _ => true) semty
	in case etvop of
	       NONE => NONE
	     | SOME ({id, scope, bind, class, lab, poly}, _, _, _) =>
	       SOME (consBindSc id bind class lab poly)
	end
      | ((ENVCON _, _, _, _), _, _, SOME _) => NONE (*(2010-02-24)lid is not a path of env*)
      | _ => NONE(*(*(2010-02-24)lid might actually be a path of env, but we've got to check*)
	(case CL.followPathStr lid (envToStrIds env) of
	     NONE => NONE
	   | SOME (str, id, lab) => SOME (consBindPoly id
						       (newEnvVar ())
						       (CL.classToSTRstr (CL.consSTR ()) str)
						       lab))*)
      (* The binding we generate is with an environment variable because then
       * in Build.sml, in buildopnsem, we can build all the environment.
       * If we can't find the identifier then we don't report any error
       * because it should have been reported when dealing with the
       * opening before. *)*)


(* Generates an environment from a long identifier and a type function *)
fun genLongEnv (I.ID (id, lab)) tyfun =
    let val tfv  = T.freshtyfvar ()
	val c    = genCstTfEm (T.consTFV tfv) tyfun lab
	val typs = singenv (id, [consBindPoly id (T.consTFV tfv, TYP, ref (emvar, false)) (CL.consTYCON ()) lab])
    in (singcst (lab, c), projTyps typs)
    end
  | genLongEnv (I.LID ((id, lab1), lid, lab2)) tyfun =
    let val (cst, env1) = genLongEnv lid tyfun
	val ev1  = freshenvvar ()
	val ev2  = freshenvvar ()
	val c1   = genCstEvEm (consEnvVar ev1 lab1) env1 lab1
	val c2   = genCstEvEm (consEnvVar ev2 lab2) (consEnvVar ev1 lab2) lab2
	val strs = singenv (id, [consBindPoly id (consEnvVar ev2 lab1) (CL.consSTR ()) lab1])
    in (conscst (lab2, c2) (conscst (lab1, c1) cst), projStrs strs)
    end


(* Checks whether the environment is composed by at least an environment variable. *)
fun hasEnvVar (ENVVAR _)            = true
  | hasEnvVar (ENVSEQ (env1, env2)) = hasEnvVar env2 orelse hasEnvVar env1
  | hasEnvVar (ENVDEP extenv)       = hasEnvVar (EL.getExtLabT extenv)
  | hasEnvVar _                     = false


(* Checks the second element in infoEnv (complete environment). *)
(* Should we check the label as well? *)
fun completeEnv (env as ENVCON _)     = getICmp env
  | completeEnv (ENVSEQ (env1, env2)) = completeEnv env2 andalso completeEnv env1
  | completeEnv (ENVDEP extenv)       = completeEnv (EL.getExtLabT extenv)
  | completeEnv  _                    = false

(*(* Marks an environment as being incomplete. *)
fun toIncompleteEnv (env as ENVCON _) = updateICmp false env
  | toIncompleteEnv x = x*)


fun isEnvV (ENVVAR _) = true
  | isEnvV _ = false

(* Checks if the environment is a ENVCON *)
fun isEnvC (ENVCON _) = true
  | isEnvC _ = false


(* Returns the labels of environments *)
(* n is just some debugging info *)
fun getLabsIdsGenEnv idenv =
    foldrienv (fn (id, semty, (lids, labs)) =>
		  foldr (fn (bind, (lids, labs)) =>
			    let val lab   = getBindL bind
				val lid   = (L.toInt lab, I.toInt id)
				val labs' = EL.getExtLabL bind
			    in (lid :: lids, L.union labs' labs)
			    end)
			(lids, labs)
			semty)
	      ([], L.empty)
	      idenv

(* Returns the ids (as integers) of an environment along with the associated labels.
 * The int is a debugging info.
 * This function is only used by our unification algorithm (in Unification.sml). *)
fun getLabsIdsEnv (env as ENVCON _) n =
    let val (idlabsVids, labsVids) = getLabsIdsGenEnv (getVids env)
	val (idlabsTyps, labsTyps) = getLabsIdsGenEnv (getTyps env)
	val (idlabsTyvs, labsTyvs) = getLabsIdsGenEnv (getTyvs env)
	val (idlabsStrs, labsStrs) = getLabsIdsGenEnv (getStrs env)
	val (idlabsSigs, labsSigs) = getLabsIdsGenEnv (getSigs env)
	val (idlabsFuns, labsFuns) = getLabsIdsGenEnv (getFuns env)
	val (idlabsOvcs, labsOvcs) = getLabsIdsGenEnv (getOvcs env)
    in (idlabsVids @ idlabsTyps @ idlabsTyvs @ idlabsStrs @ idlabsSigs @ idlabsFuns @ idlabsOvcs,
	L.unions [labsVids, labsTyps, labsTyvs, labsStrs, labsSigs, labsFuns, labsOvcs])
    end
  | getLabsIdsEnv _ _ = raise EH.DeadBranch "" (*([], L.empty)*)


(* Returns the label associated to a environment (dummylab if not an ENVCON).
 * This function is only used for complete environments which are ENVCONs.
 * It is only used in Unification.sml.*)
fun getLabEnv (env as ENVCON _) = getILab env
  | getLabEnv _                 = raise EH.DeadBranch "" (*L.dummyLab*)




(* Extract the type names defined in a type constructor environment *)
fun getTyNames typs =
    foldrienv (fn (id, sem, tnmap) =>
		  List.foldr (fn (bind, tnmap) =>
				 let val (tf, tnKind, _) = getBindT bind
				     val lab = getBindL bind
				     val (names, labs, stts, deps) = T.isTyName tf
				 in case names of
					T.TYNAME name =>
					let val labs  = L.union  labs (EL.getExtLabL bind)
					    val stts  = L.union  stts (EL.getExtLabE bind)
					    val deps  = CD.union deps (EL.getExtLabD bind)
					    val tname = {id = id, lab = lab, kind = tnKind, name = name}
					    val names = TYNAME (tname, labs, stts, deps)
					in names :: tnmap
					end
				      | T.DUMTYNAME name =>
					let val tname = {id = id, lab = lab, kind = tnKind, name = name}
					    val names = DUMTYNAME tname
					in names :: tnmap
					end
				      | T.MAYTYNAME => tnmap
				      | T.NOTTYNAME =>
					let val labs  = L.union  labs (EL.getExtLabL bind)
					    val stts  = L.union  stts (EL.getExtLabE bind)
					    val deps  = CD.union deps (EL.getExtLabD bind)
					    val names = NOTTYNAME (id, labs, stts, deps)
					in names :: tnmap
					end
				 end)
			     tnmap
			     sem)
	      []
	      typs



(* The set of labels is the set of labels that we want to keep *)

(* Same as in Filter *)
fun testlab lab labs = L.eq lab L.dummyLab
		       orelse L.eq lab L.builtinLab
		       orelse L.isin lab labs

fun testlabs labs1 labs2 = L.subseteq labs1 (L.cons L.dummyLab (L.cons L.builtinLab labs2))

fun filterIdEnv idenv labs =
    OME.foldri (fn (id, sem, (idenv, cmp)) =>
		   let val (sem', cmp') =
			   foldr (fn (bind, (sem, cmp)) =>
				     if testlabs (EL.getExtLabL bind) labs
				     then (bind :: sem, cmp)
				     else (sem, cmp(*false*)))
				 ([], cmp)
				 sem
		   in if List.null sem'
		      then (idenv, false)
		      else (addenv (id, sem') idenv, cmp')
		   end)
	       (emgen, true)
	       idenv

fun filterOpnEnv opnenv labs =
    let val opnenv' =
	    OMO.foldr (fn (opnsem as (lid, lab, kind), opnenv) =>
			  if testlab lab labs
			  then addOEnv opnsem opnenv
			  else opnenv)
		      emopn
		      opnenv
    in if OMO.isEmpty opnenv'
       then NONE
       else SOME opnenv'
    end

fun filterLongTyp (longtyp as ({lid, sem, class, lab}, _, _, _)) labs =
    if testlab lab labs
    then SOME longtyp
    else NONE

fun toIncomplete (env as ENVCON _)               = updateICmp false env
  | toIncomplete (env as ENVSEQ (env1, env2))    = ENVSEQ (toIncomplete env1, toIncomplete env2)
  | toIncomplete (env as ENVVAR _)               = env
  | toIncomplete (env as ENVLOC (env1, env2))    = ENVSEQ (env1, toIncomplete env2)
  | toIncomplete (env as ENVWHR (env1, longtyp)) = ENVWHR (toIncomplete env1, longtyp)
  | toIncomplete (env as ENVSHA (env1, env2))    = ENVSHA (toIncomplete env1, env2)
  | toIncomplete (env as ENVSIG (env1, env2, l)) = ENVSIG (toIncomplete env1, env2, l)
  | toIncomplete (env as ENVPOL (tyvenv, env1))  = ENVPOL (tyvenv, toIncomplete env1)
  | toIncomplete (env as ENVDAT (idl, env1))     = ENVDAT (idl, toIncomplete env1)
  | toIncomplete (env as ENVOPN _)               = env
  | toIncomplete (env as ENVDEP eenv)            = ENVDEP (EL.mapExtLab eenv toIncomplete)
  | toIncomplete (env as ENVFUN _)               = env
  | toIncomplete (env as ENVCST _)               = env
  | toIncomplete (env as ENVPTY _)               = env
  | toIncomplete (env as ENVFIL (f, e, strm))    = ENVFIL (f, toIncomplete e, fn () => toIncomplete (strm ()))
  | toIncomplete (env as ENVTOP)                 = env

fun filterOcst (ocst as CSTTYP _) labs = SOME ocst
  | filterOcst (ocst as CSTTYN _) labs = SOME ocst
  | filterOcst (ocst as CSTSEQ _) labs = SOME ocst
  | filterOcst (ocst as CSTROW _) labs = SOME ocst
  | filterOcst (ocst as CSTLAB _) labs = SOME ocst
  | filterOcst (ocst as CSTENV ((env1, env2), labs0, stts0, deps0)) labs =
    (case (filterEnv env1 labs, filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (CSTENV ((env1', env2'), labs0, stts0, deps0))
       | (SOME env1', NONE)       => SOME (CSTENV ((env1', emenv), labs0, stts0, deps0))
       | (NONE,       SOME env2') => SOME (CSTENV ((emenv, env2'), labs0, stts0, deps0))
       | (NONE,       NONE)       => NONE)
  | filterOcst (ocst as CSTCLS _) labs = SOME ocst
  | filterOcst (ocst as CSTTYF _) labs = SOME ocst
  | filterOcst (ocst as CSTACC _) labs = SOME ocst
  | filterOcst (ocst as CSTLET env) labs =
    (case filterEnv env labs of
	 SOME env' => SOME (CSTLET env')
       | NONE      => NONE)
  | filterOcst (ocst as CSTSIG _) labs = SOME ocst
  | filterOcst (ocst as CSTFUN _) labs = SOME ocst
  | filterOcst (ocst as CSTSHA _) labs = SOME ocst

and filterCst (OCST ocst) labs =
    OCST (OMC.mapPartiali (fn (key, cs) =>
			      if testlab (L.fromInt key) labs
			      then case List.mapPartial (fn ocst => filterOcst ocst labs) cs of
				       []  => NONE
				     | cs' => SOME cs'
			      else NONE)
			  ocst)

and filterEnv (env as ENVCON _) labs =
    let val (vids, cmpVids) = filterIdEnv (getVids env) labs
	val (typs, cmpTyps) = filterIdEnv (getTyps env) labs
	val (tyvs, cmpTyvs) = filterIdEnv (getTyvs env) labs
	val (strs, cmpStrs) = filterIdEnv (getStrs env) labs
	val (sigs, cmpSigs) = filterIdEnv (getSigs env) labs
	val (funs, cmpFuns) = filterIdEnv (getFuns env) labs
	val (ovcs, cmpOvcs) = filterIdEnv (getOvcs env) labs
	val cmp  = cmpVids andalso cmpTyps andalso cmpTyvs andalso
		   cmpStrs andalso cmpSigs andalso cmpFuns andalso
		   cmpOvcs andalso getICmp env
	val info = consInfo (getILab env) cmp (getITns env) (getIFct env)
	val env' = consEnvC vids typs tyvs strs sigs funs ovcs info
    in (*if isEmptyEnv env'
       then NONE
       else*) SOME env'
    end
  | filterEnv (env as ENVVAR _) labs = SOME env
  | filterEnv (env as ENVSEQ (env1, env2)) labs =
    (case (filterEnv env1 labs, filterEnv env2 labs) of
	 (SOME env1', SOME env2') =>
	 (*if isEmptyEnv env1'
	 then SOME (toIncomplete env2')
	 else if isEmptyEnv env2'
	 then SOME (toIncomplete env1')
	 else*) SOME (ENVSEQ (env1', env2'))
       | (SOME env1', NONE)       => SOME env1'
       | (NONE,       SOME env2') => SOME env2'
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as ENVLOC (env1, env2)) labs =
    (case (filterEnv env1 labs , filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (ENVLOC (env1', env2'))
       | (SOME env1', NONE)       => SOME (ENVLOC (env1', updateICmp false emenv))
       | (NONE,       SOME env2') => SOME env2'
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as ENVWHR (env1, longtyp)) labs =
    (case (filterEnv env1 labs, filterLongTyp longtyp labs) of
	 (SOME env1', SOME longtyp') => SOME (ENVWHR (env1', longtyp'))
       | (SOME env1', NONE)          => SOME env1'
       | (NONE,       SOME longtyp') => SOME (ENVWHR (updateICmp false emenv, longtyp'))
       | (NONE,       NONE)          => NONE)
  | filterEnv (env as ENVSHA (env1, env2)) labs =
    (case (filterEnv env1 labs , filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (ENVSHA (env1', env2'))
       | (SOME env1', NONE)       => SOME env1'
       | (NONE,       SOME env2') => SOME (ENVSHA (updateICmp false emenv, env2'))
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as ENVSIG (env1, env2, kind)) labs =
    (case (filterEnv env1 labs , filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (ENVSIG (env1', env2', kind))
       | (SOME env1', NONE)       => SOME (ENVSIG (env1', updateICmp false emenv, kind))
       | (NONE,       SOME env2') => SOME (ENVSIG (updateICmp false emenv, env2', kind))
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as ENVPOL (tyvenv, env1)) labs =
    (case (filterIdEnv tyvenv labs , filterEnv env1 labs) of
	 ((tyvenv', _), SOME env1') => SOME (ENVPOL (tyvenv', env1'))
       | ((tyvenv', _), NONE)       =>
	 if isEmptyIdEnv tyvenv'
	 then NONE
	 else SOME (ENVPOL (tyvenv', updateICmp false emenv)))
  | filterEnv (env as ENVDAT (idl as (id, lab), env1)) labs =
    (case filterEnv env1 labs of
	 SOME env1' => SOME (ENVDAT (idl, env1'))
       | NONE       => if testlab lab labs
		       then SOME (ENVDAT (idl, updateICmp false emenv))
		       else NONE)
  | filterEnv (env as ENVOPN opnenv) labs =
    (case filterOpnEnv opnenv labs of
	 SOME opnenv' => SOME (ENVOPN opnenv')
       | NONE         => NONE)
  | filterEnv (env as ENVDEP (env1, labs1, stts1, deps1)) labs =
    if testlabs labs1 labs
    then case filterEnv env1 labs of
	     SOME env1' => SOME (ENVDEP (env1', labs1, stts1, deps1))
	   | NONE       => NONE
    else NONE
  | filterEnv (env as ENVFUN cst) labs =
    (case filterCst cst labs of
	 OCST ocsts => if OMC.numItems ocsts = 0
		       then NONE
		       else SOME (ENVFUN (OCST ocsts)))
  | filterEnv (env as ENVCST cst) labs =
    (case filterCst cst labs of
	 OCST ocsts => if OMC.numItems ocsts = 0
		       then NONE
		       else SOME (ENVCST (OCST ocsts)))
  | filterEnv (env as ENVPTY _) _ = SOME env
  | filterEnv (env as ENVFIL (file, e, strm)) labs =
    (case (filterEnv e labs, filterEnv (strm ()) labs) of
	 (SOME env1, SOME env2) => SOME (ENVFIL (file, env1, fn () => env2))
       | (SOME env1, NONE)      => SOME (ENVFIL (file, env1, fn () => emenv))
       | (NONE,      SOME env2) => SOME (ENVFIL (file, emenv, fn () => env2))
       | (NONE,      NONE)      => NONE)
  | filterEnv (env as ENVTOP) _ = SOME env


val filterEnv = fn env => fn labs => case filterEnv env labs of NONE => updateICmp false emenv | SOME env' => env'


(*fun getAllTns (env as ENVCON _) =
    foldrenv (fn (sem, tns) => foldr (fn (bind, tns) => (getAllTns (getBindT bind)) @ tns) tns sem)
	     (getITns env)
	     (getStrs env)
  | getAllTns (ENVSEQ (env1, env2)) = (getAllTns env1) @ (getAllTns env2)
  | getAllTns (ENVVAR _) = []
  | getAllTns _ = raise EH.DeadBranch ""*)


(*(* Returns the variable contained in the ENVVAR if the environment is a ENVVAR. *)
fun getEnvVar (ENVVAR ev) = SOME ev
  | getEnvVar _         = NONE


(* Generates INJVID/INJTYP constraints from an Id.lid and an envvar *)

(*fun generateINJ (I.ID (id, lab)) ty class ev labs deps asmp =
    let val finj = if CL.classIsVID class
		   then INJVID
		   else if CL.classIsTYCON class
		   then INJTYP
		   else raise EH.DeadBranch "wrong kind of identifier"
	val c = genCstEvAll (ENVVAR ev)
			    (finj (C.consConsId' id ty class lab))
			    labs
			    deps
			    asmp
    in [c] end
  | generateINJ (I.LID ((id, lab), lid, _)) ty class ev labs deps asmp =
    let val ev' = freshenvvar ()
	val env = generateINJ lid ty class ev' labs deps asmp
	val c   = genCstEvAll (ENVVAR ev)
			      (INJSTR (EENV (C.consConsId' id (ENVVAR ev') (CL.consSTR ()) lab)))
			      labs
			      deps
			      asmp
    in c :: cs end*)

fun generateINJ' (I.ID (id, lab)) sem class finj =
    finj (consBindPoly id sem class lab)
  | generateINJ' (I.LID ((id, lab), lid, _)) sem class finj =
    let val env = generateINJ' lid sem class finj
    in INJSTR (consBindPoly id env (CL.consSTR ()) lab)
    end

fun generateINJ lid sem class =
    let val finj = if CL.classIsVID class
		   then INJVID
		   else if CL.classIsTYCON class
		   then INJTYP
		   else raise EH.DeadBranch "wrong kind of identifier"
    in generateINJ' lid sem class finj
    end

(* replace all the labels of the top level constraints by the dummy label *)

fun dummysCst cst =
    foldricst
	(fn (_, ocst, cst) => conscstSems (L.dummylab, ocst) cst)
	emcst
	cst

fun dummysCs (css, cst) = (css, dummysCst cst)


(* transforms a T.chkty into a status *)

fun vidToStatus (CL.VAL _) = I
  | vidToStatus (CL.PAT _) = I
  | vidToStatus (CL.CON _) = D
  | vidToStatus (CL.REC _) = V
  | vidToStatus (CL.EXC _) = E
  | vidToStatus _ = raise EH.DeadBranch ""

fun chkToStatus (CL.VID vid, _) = vidToStatus vid
  | chkToStatus _               = raise EH.DeadBranch "not a identifier status"*)



end
