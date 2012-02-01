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
		    | SEQUENCE_ENV of env * env
		    | ENVLOC of env * env
		    | ENVWHR of env * longtyp
		    | ENVSHA of env * env
		    | SIGNATURE_ENV of env * env * matchKind
		    | ENVPOL of tyvenv * env
		    | DATATYPE_CONSTRUCTOR_ENV of I.idl  * env
		    | ENVOPN of opnenv
		    | ENVDEP of env EL.extLab
		    | FUNCTOR_ENV of cst
		    | CONSTRAINT_ENV of cst
		    | ENVPTY of string
		    | ENVFIL of string * env * (unit -> env)
		    | TOP_LEVEL_ENV

     and acc        = ACCVAR of T.ty        accid EL.extLab
		    | ACCETV of T.tyvar     accid EL.extLab
		    | ACCTYP of T.tyfun     accid EL.extLab
		    | ACCOVC of T.seqty     accid EL.extLab
		    | ACCSTR of env         accid EL.extLab
		    | ACCSIG of env         accid EL.extLab
		    | ACCFUN of (env * env) accid EL.extLab

     and ocst 	    = TYPE_CONSTRAINT     of (T.ty     * T.ty)     EL.extLab
		    | TYPENAME_CONSTRAINT of (T.tnty   * T.tnty)   EL.extLab
		    | SEQUENCE_CONSTRAINT of (T.seqty  * T.seqty)  EL.extLab
		    | ROW_CONSTRAINT of (T.rowty  * T.rowty)  EL.extLab
		    | LABEL_CONSTRAINT of (T.labty  * T.labty)  EL.extLab
		    | ENV_CONSTRAINT of (env      * env)      EL.extLab
		    | IDENTIFIER_CLASS_CONSTRAINT of (CL.class * CL.class) EL.extLab
		    | FUNCTION_TYPE_CONSTRAINT of (T.tyfun  * T.tyfun)  EL.extLab
		    | ACCESSOR_CONSTRAINT of acc
		    | LET_CONSTRAINT of env
		    | SIGNATURE_CONSTRAINT of evsbind
		    | FUNCTOR_CONSTRAINT of evfbind
		    | SHARING_CONSTRAINT of shabind
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

fun printOp NONE _ = "-"
  | printOp (SOME x) f = f x

fun printEvOp       x = printOp x printEnvVar

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

fun printTnmap xs =
    printlistgen xs (fn {id, lab, kind, name} =>
			"(" ^ I.printId     id   ^
			"," ^ L.printLab    lab  ^
			"," ^ printTnKind   kind ^
			"," ^ T.printtyname name ^ ")")

fun printNfoEnv {lab, cmp, tns, fct} =
    "(" ^ L.printLab    lab ^
    "," ^ Bool.toString cmp ^
    "," ^ printTnmap    tns ^
    "," ^ Bool.toString fct ^ ")"

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
  | printEnv (FUNCTOR_ENV cst) ind = "CONSTRAINT_ENV(" ^ printcst' cst ind I.emAssoc ^ ")"
  | printEnv (CONSTRAINT_ENV cst) ind = "CONSTRAINT_ENV(" ^ printcst' cst ind I.emAssoc ^ ")"
  | printEnv (SEQUENCE_ENV (env1, env2)) ind =
    "SEQUENCE_ENV(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (ENVLOC (env1, env2)) ind =
    "ENVLOC(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (ENVWHR (env, longtyp)) ind =
    "ENVWHR(" ^ printEnv env ind ^ ",\n" ^ ind ^ printLongTyp longtyp ind ^ ")"
  | printEnv (ENVSHA (env1, env2)) ind =
    "ENVSHA(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (SIGNATURE_ENV (env1, env2, kind)) ind =
    "SIGNATURE_ENV(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ "," ^ printMatchKind kind ^ ")"
  | printEnv (ENVPOL (tyvenv, env)) ind =
    "ENVPOL(" ^ printTyvEnv tyvenv ind ^ ",\n" ^ ind ^ printEnv env ind ^ ")"
  | printEnv (DATATYPE_CONSTRUCTOR_ENV (idl, env)) ind =
    "DATATYPE_CONSTRUCTOR_ENV(" ^ I.printIdL idl ^ "," ^ printEnv env ind ^ ")"
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
and printocst (TYPE_CONSTRAINT x) _ ascid =
    "  TYP(" ^ EL.printExtLab x (fn x => printPair x T.printty') ascid ^ ")"
  | printocst (FUNCTION_TYPE_CONSTRAINT x) _ ascid =
    "  TYF(" ^ EL.printExtLab x (fn x => printPair x T.printtyf') ascid ^ ")"
  | printocst (TYPENAME_CONSTRAINT x) _ ascid =
    "  NAM(" ^ EL.printExtLab x (fn x => printPair x T.printtnty') ascid ^ ")"
  | printocst (SEQUENCE_CONSTRAINT x) _ ascid =
    "  SEQ(" ^ EL.printExtLab x (fn x => printPair x T.printseqty') ascid ^ ")"
  | printocst (ROW_CONSTRAINT x) _ ascid =
    "  REC(" ^ EL.printExtLab x (fn x => printPair x T.printrowty') ascid ^ ")"
  | printocst (LABEL_CONSTRAINT x) _ ascid =
    "  LAB(" ^ EL.printExtLab x (fn x => printPair x T.printlabty) ascid ^ ")"
  | printocst (ENV_CONSTRAINT x) ind ascid =
    "  ENV(" ^ EL.printExtLab x (fn x => printPair x (fn x => printEnv x (ind ^ tab))) ascid ^ ")"
  | printocst (IDENTIFIER_CLASS_CONSTRAINT x) ind ascid =
    "  CLS(" ^ EL.printExtLab x (fn x => printPair x CL.toString) ascid  ^ ")"
  | printocst (SIGNATURE_CONSTRAINT evsbind) ind ascid = "  SIG(" ^ printEvsBind evsbind ^ ")"
  | printocst (FUNCTOR_CONSTRAINT evfbind) ind ascid = "  FUN(" ^ printEvfBind evfbind ^ ")"
  | printocst (SHARING_CONSTRAINT shabind) ind ascid = "  SHA(" ^ printShaBind shabind ^ ")"
  | printocst (LET_CONSTRAINT env)     ind ascid = "  LET(" ^ printEnv env (ind ^ "      ") ^ ")"
  | printocst (ACCESSOR_CONSTRAINT acc)     ind ascid = "  ACC(" ^ printAcc acc ind ascid ^ ")"
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

(* Bindings constructors *)

fun consBind     id bind class lab poly = EL.initExtLab (C.consBind     id bind class lab poly) lab
fun consBindPoly id bind class lab      = EL.initExtLab (C.consBindPoly id bind class lab)      lab
fun consBindMono id bind class lab      = EL.initExtLab (C.consBindMono id bind class lab)      lab

fun consAccId lid sem class lab = {lid = lid, sem = sem, class = class, lab = lab}


(* Accessors to a extenv *)

fun getBindI x = C.getBindI (EL.getExtLabT x)
fun getBindT x = C.getBindT (EL.getExtLabT x)
fun getBindC x = C.getBindC (EL.getExtLabT x)
fun getBindL x = C.getBindL (EL.getExtLabT x)
fun getBindP x = C.getBindP (EL.getExtLabT x)

val nextEnvVar     = ref 0
fun setNextEnvVar     n  = nextEnvVar := n
fun getEnvVar   () = !nextEnvVar
fun freshEnvVar () = let val x = !nextEnvVar in nextEnvVar := x + 1; x end
fun resetEnvVar () = setNextEnvVar 0

fun envVarToInt envvar = envvar

fun eqEnvVar ev1 ev2 = (ev1 = (ev2 : envvar))

fun consENVVAR ev lab = ENVVAR (ev, lab)
fun newEnvVar  lab    = consENVVAR (freshEnvVar ()) lab

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
  | getITns (SEQUENCE_ENV (e1, e2)) = (getITns e1) @ (getITns e2)
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
  | updateILab lab (SEQUENCE_ENV (env1, env2)) = SEQUENCE_ENV (updateILab lab env1, updateILab lab env2)
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
  | updateIFct fct (SEQUENCE_ENV (env1, env2)) = SEQUENCE_ENV (updateIFct fct env1, updateIFct fct env2)
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
  | isEmptyEnv (SEQUENCE_ENV (env1, env2)) = isEmptyEnv env1 andalso isEmptyEnv env2
  | isEmptyEnv env = false

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
    else SEQUENCE_ENV (env, envsToSeq envs)

val foldlOEnv = OMO.foldl
fun appOEnv fapp env = OMO.app fapp env
fun addOEnv x env = OMO.enqueue (env, x)
fun singOEnv x = addOEnv x emopn
fun uOEnv envl =
    foldl (fn (env1, env2) => OMO.foldl (fn (x, env) => addOEnv x env) env2 env1)
	  emopn
	  envl

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
    else SEQUENCE_ENV (x, y)

fun uenvEnv [] = emenv
  | uenvEnv ((SEQUENCE_ENV (env1, env2)) :: xs) = uenvEnv (env1 :: env2 :: xs)
  | uenvEnv [x] = x
  | uenvEnv (x :: xs) = uenvEnvC x (uenvEnv xs)

fun closeVids vids clos =
    mapenv (fn sem => map (fn x => EL.mapExtLab x (fn x => C.closeBind x clos)) sem)
	   vids

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
  | plusEnv (SEQUENCE_ENV (env1, env2)) env3 = SEQUENCE_ENV (env1, plusEnv env2 env3)
  | plusEnv env1 (SEQUENCE_ENV (env2, env3)) = SEQUENCE_ENV (plusEnv env1 env2, env3)
  | plusEnv env1 env2 =
    if isEmptyEnv env1
    then env2
    else if isEmptyEnv env2
    then env1
    else SEQUENCE_ENV (env1, env2)

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
  | pushExtEnv (SEQUENCE_ENV (env1, env2)) labs stts deps =
    SEQUENCE_ENV (pushExtEnv env1 labs stts deps, pushExtEnv env2 labs stts deps)
  (*(2010-06-09)NOTE: we shouldn't need to push onto env1 because this environment
   * should be useless. *)
  | pushExtEnv (ENVDEP (env, labs0, stts0, deps0)) labs stts deps =
    pushExtEnv env (L.union labs0 labs) (L.union stts0 stts) (CD.union deps0 deps)
  | pushExtEnv env labs stts deps = ENVDEP (env, labs, stts, deps)

val emcss = []
val emcst = OCST OMC.empty

fun getcstSemi cst i = case OMC.find (cst, i) of NONE => [] | SOME x => x

fun conscss c css = c :: css
fun conscst (v, c) (OCST cst) = OCST (OMC.insert (cst, L.toInt v, c :: (getcstSemi cst (L.toInt v))))
fun conscsss cl css = cl @ css
fun conscsts (v, cs) (OCST cst) = OCST (OMC.insert (cst, L.toInt v, cs @ (getcstSemi cst (L.toInt v))))

fun singcss  c  = [c]
fun singcsss cs = cs
fun singcst  (v, c)  = conscst  (v, c)  emcst
fun singcsts (v, cs) = conscsts (v, cs) emcst

fun uenv2css css1 css2 = css1 @ css2
fun uenvcss xs = foldr (fn (x, y) => uenv2css x y) emcss xs
fun uenv2cst (OCST cst1) (OCST cst2) = OCST (OMC.unionWith (fn (x, y) => x @ y) (cst1, cst2))
fun uenvcst xs = foldr (fn (x, y) => uenv2cst x y) emcst xs

fun foldlicst ffold init (OCST cst) = OMC.foldli ffold init cst
fun foldricst ffold init (OCST cst) = OMC.foldri ffold init cst
fun mapicst   ffold      (OCST cst) = OCST (OMC.mapi ffold cst)

fun getnbcst' cst =
    foldlicst (fn (_, cs, nb) =>
		  foldl (fn (TYPE_CONSTRAINT _, nb) => 1 + nb
			  | (TYPENAME_CONSTRAINT _, nb) => 1 + nb
			  | (SEQUENCE_CONSTRAINT _, nb) => 1 + nb
			  | (ROW_CONSTRAINT _, nb) => 1 + nb
			  | (LABEL_CONSTRAINT _, nb) => 1 + nb
			  | (IDENTIFIER_CLASS_CONSTRAINT _, nb) => 1 + nb
			  | (FUNCTION_TYPE_CONSTRAINT _, nb) => 1 + nb
			  | (ACCESSOR_CONSTRAINT _, nb) => 1 + nb
			  | (SIGNATURE_CONSTRAINT _, nb) => 1 + nb
			  | (FUNCTOR_CONSTRAINT _, nb) => 1 + nb
			  | (SHARING_CONSTRAINT _, nb) => 1 + nb
			  | (ENV_CONSTRAINT ((env1, env2), _, _, _), nb) => (getnbcstenv env1) + (getnbcstenv env2) + nb
			  | (LET_CONSTRAINT env, nb) => (getnbcstenv env) + nb)
			nb
			cs)
	      0
	      cst

and getnbcstenv (ENVCON _) = 0
  | getnbcstenv (ENVVAR _) = 0
  | getnbcstenv (SEQUENCE_ENV (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVLOC (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVSHA (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (SIGNATURE_ENV (env1, env2, _)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVWHR (env, _)) = getnbcstenv env
  | getnbcstenv (ENVPOL (_, env)) = getnbcstenv env
  | getnbcstenv (DATATYPE_CONSTRUCTOR_ENV (_, env)) = getnbcstenv env
  | getnbcstenv (ENVOPN _) = 0
  | getnbcstenv (ENVDEP eenv) = getnbcstenv (EL.getExtLabT eenv)
  | getnbcstenv (FUNCTOR_ENV cst)  = getnbcst' cst
  | getnbcstenv (CONSTRAINT_ENV cst)  = getnbcst' cst
  | getnbcstenv (ENVPTY _) = 0
  | getnbcstenv (ENVFIL (file, env, strm)) = (getnbcstenv env) + (getnbcstenv (strm ()))
  | getnbcstenv ENVTOP = 0

fun getnbcst (env, css) = getnbcstenv env

fun getnbcsttop (env, css) = getnbcstenv env (* NOTE: this not useful anymore *)

fun getnbcss' css = List.length css

fun getnbcss (envc, css) = getnbcss' css

fun getnbcs (env, css) = (getnbcstenv env) + (getnbcss' css)

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
  | getlabsenv (SEQUENCE_ENV (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVLOC (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVSHA (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (SIGNATURE_ENV (env1, env2, _)) = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVWHR (env, _))        = getlabsenv env (*Don't we want to get the label of the longtyp?*)
  | getlabsenv (ENVPOL (tyvenv, env))   = combineLabsEnv ([getlabsidenv tyvenv], L.empty) (getlabsenv env)
  | getlabsenv (DATATYPE_CONSTRUCTOR_ENV (idl, env))      = getlabsenv env
  | getlabsenv (ENVOPN opnenv)          = ([getlabopnenv opnenv], L.empty)
  | getlabsenv (ENVDEP eenv)            = getlabsenv (EL.getExtLabT eenv)
  | getlabsenv (FUNCTOR_ENV cst)             = getlabscst cst
  | getlabsenv (CONSTRAINT_ENV cst)             = getlabscst cst
  | getlabsenv (ENVVAR _)               = ([], L.empty)
  | getlabsenv (ENVPTY _)               = ([], L.empty)
  | getlabsenv (ENVFIL (f, env, strm))  = combineLabsEnv (getlabsenv env) (getlabsenv (strm ()))
  | getlabsenv ENVTOP                   = ([], L.empty)
and getlabcsbindocst (TYPE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (FUNCTION_TYPE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (TYPENAME_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (SEQUENCE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (ROW_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (LABEL_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (ENV_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (IDENTIFIER_CLASS_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (ACCESSOR_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (LET_CONSTRAINT env) = getlabsenv env
  | getlabcsbindocst (SIGNATURE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (FUNCTOR_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (SHARING_CONSTRAINT _)   = ([], L.empty)
and getlabscst cst =
    foldricst
	(fn (_, ocstl, labsenv) =>
	    foldr (fn (ocst, labsenv) => combineLabsEnv (getlabcsbindocst ocst) labsenv)
		  labsenv
		  ocstl)
	([], L.empty)
	cst
and getbindings env = getlabsenv env

(* ====== Generation of type constraints ====== *)


fun genCstAllGen x1 x2 labs sts cds = EL.consExtLab (x1, x2) labs sts cds
fun genCstTyAll x1 x2 labs sts cds = TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstTfAll x1 x2 labs sts cds = FUNCTION_TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstTnAll x1 x2 labs sts cds = (D.printDebug 3 D.ENV ("in genCstTnAll - x1 = "^(T.printtnty x1)^", x2 = "^(T.printtnty x2));
						  TYPENAME_CONSTRAINT (genCstAllGen x1 x2 labs sts cds))
fun genCstSqAll x1 x2 labs sts cds = SEQUENCE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstRtAll x1 x2 labs sts cds = ROW_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstLtAll x1 x2 labs sts cds = LABEL_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstEvAll x1 x2 labs sts cds = ENV_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstClAll x1 x2 labs sts cds = IDENTIFIER_CLASS_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)

fun genAccAllGen x labs sts cds = EL.consExtLab x labs sts cds

fun genAccIvAll x labs sts cds = ACCVAR (genAccAllGen x labs sts cds)
fun genAccIeAll x labs sts cds = ACCETV (genAccAllGen x labs sts cds)
fun genAccItAll x labs sts cds = ACCTYP (genAccAllGen x labs sts cds)
fun genAccIoAll x labs sts cds = ACCOVC (genAccAllGen x labs sts cds)
fun genAccIsAll x labs sts cds = ACCSTR (genAccAllGen x labs sts cds)
fun genAccIiAll x labs sts cds = ACCSIG (genAccAllGen x labs sts cds)
fun genAccIfAll x labs sts cds = ACCFUN (genAccAllGen x labs sts cds)


fun genCstGen x1 x2 lab cds = EL.consExtLab (x1, x2) (L.singleton lab) L.empty cds

fun genCstTy x1 x2 lab cds = TYPE_CONSTRAINT (genCstGen x1 x2 lab cds)
fun genCstTf x1 x2 lab cds = FUNCTION_TYPE_CONSTRAINT (genCstGen x1 x2 lab cds)
fun genCstTn x1 x2 lab cds = (D.printDebug 3 D.ENV ("in genCstTn - x1 = "^(T.printtnty x1)^", x2 = "^(T.printtnty x2));
					  TYPENAME_CONSTRAINT (genCstGen x1 x2 lab cds))
fun genCstSq x1 x2 lab cds = SEQUENCE_CONSTRAINT (genCstGen x1 x2 lab cds)
fun genCstRt x1 x2 lab cds = ROW_CONSTRAINT (genCstGen x1 x2 lab cds)
fun genCstLt x1 x2 lab cds = LABEL_CONSTRAINT (genCstGen x1 x2 lab cds)
fun genCstEv x1 x2 lab cds = ENV_CONSTRAINT (genCstGen x1 x2 lab cds)
fun genCstCl x1 x2 lab cds = IDENTIFIER_CLASS_CONSTRAINT (genCstGen x1 x2 lab cds)

fun genAccGen x lab cds = EL.consExtLab x (L.singleton lab) L.empty cds

fun genAccIv x lab cds = ACCVAR (genAccGen x lab cds)
fun genAccIe x lab cds = ACCETV (genAccGen x lab cds)
fun genAccIt x lab cds = ACCTYP (genAccGen x lab cds)
fun genAccIo x lab cds = ACCOVC (genAccGen x lab cds)
fun genAccIs x lab cds = ACCSTR (genAccGen x lab cds)
fun genAccIi x lab cds = ACCSIG (genAccGen x lab cds)
fun genAccIf x lab cds = ACCFUN (genAccGen x lab cds)


fun initTypeConstraint x1 x2 lab = (TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab))
fun initFunctionTypeConstraint x1 x2 lab = FUNCTION_TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initTypenameConstraint x1 x2 lab = TYPENAME_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initSequenceConstraint x1 x2 lab = SEQUENCE_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initRowConstraint x1 x2 lab = ROW_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initLabelConstraint x1 x2 lab = LABEL_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initEnvConstraint x1 x2 lab = ENV_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initClassConstraint x1 x2 lab = IDENTIFIER_CLASS_CONSTRAINT (EL.initExtLab (x1, x2) lab)

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
				  val c   = initTypeConstraint ty ty' lab
			      in conscst (lab, c) cst
			      end)
			  cst
			  sems)
		emcst
		vids
    end

(* Generates an environment from a long identifier and a type function *)
fun genLongEnv (I.ID (id, lab)) tyfun =
    let val tfv  = T.freshtyfvar ()
	val c    = initFunctionTypeConstraint (T.consTFV tfv) tyfun lab
	val typs = singenv (id, [consBindPoly id (T.consTFV tfv, TYP, ref (emvar, false)) (CL.consTYCON ()) lab])
    in (singcst (lab, c), projTyps typs)
    end
  | genLongEnv (I.LID ((id, lab1), lid, lab2)) tyfun =
    let val (cst, env1) = genLongEnv lid tyfun
	val ev1  = freshEnvVar ()
	val ev2  = freshEnvVar ()
	val c1   = initEnvConstraint (consENVVAR ev1 lab1) env1 lab1
	val c2   = initEnvConstraint (consENVVAR ev2 lab2) (consENVVAR ev1 lab2) lab2
	val strs = singenv (id, [consBindPoly id (consENVVAR ev2 lab1) (CL.consSTR ()) lab1])
    in (conscst (lab2, c2) (conscst (lab1, c1) cst), projStrs strs)
    end


(* Checks whether the environment is composed by at least an environment variable. *)
fun hasEnvVar (ENVVAR _)            = true
  | hasEnvVar (SEQUENCE_ENV (env1, env2)) = hasEnvVar env2 orelse hasEnvVar env1
  | hasEnvVar (ENVDEP extenv)       = hasEnvVar (EL.getExtLabT extenv)
  | hasEnvVar _                     = false


(* Checks the second element in infoEnv (complete environment). *)
(* Should we check the label as well? *)
fun completeEnv (env as ENVCON _)     = getICmp env
  | completeEnv (SEQUENCE_ENV (env1, env2)) = completeEnv env2 andalso completeEnv env1
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
  | toIncomplete (env as SEQUENCE_ENV (env1, env2))    = SEQUENCE_ENV (toIncomplete env1, toIncomplete env2)
  | toIncomplete (env as ENVVAR _)               = env
  | toIncomplete (env as ENVLOC (env1, env2))    = SEQUENCE_ENV (env1, toIncomplete env2)
  | toIncomplete (env as ENVWHR (env1, longtyp)) = ENVWHR (toIncomplete env1, longtyp)
  | toIncomplete (env as ENVSHA (env1, env2))    = ENVSHA (toIncomplete env1, env2)
  | toIncomplete (env as SIGNATURE_ENV (env1, env2, l)) = SIGNATURE_ENV (toIncomplete env1, env2, l)
  | toIncomplete (env as ENVPOL (tyvenv, env1))  = ENVPOL (tyvenv, toIncomplete env1)
  | toIncomplete (env as DATATYPE_CONSTRUCTOR_ENV (idl, env1))     = DATATYPE_CONSTRUCTOR_ENV (idl, toIncomplete env1)
  | toIncomplete (env as ENVOPN _)               = env
  | toIncomplete (env as ENVDEP eenv)            = ENVDEP (EL.mapExtLab eenv toIncomplete)
  | toIncomplete (env as FUNCTOR_ENV _)               = env
  | toIncomplete (env as CONSTRAINT_ENV _)               = env
  | toIncomplete (env as ENVPTY _)               = env
  | toIncomplete (env as ENVFIL (f, e, strm))    = ENVFIL (f, toIncomplete e, fn () => toIncomplete (strm ()))
  | toIncomplete (env as ENVTOP)                 = env

fun filterOcst (ocst as TYPE_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as TYPENAME_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as SEQUENCE_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as ROW_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as LABEL_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as ENV_CONSTRAINT ((env1, env2), labs0, stts0, deps0)) labs =
    (case (filterEnv env1 labs, filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (ENV_CONSTRAINT ((env1', env2'), labs0, stts0, deps0))
       | (SOME env1', NONE)       => SOME (ENV_CONSTRAINT ((env1', emenv), labs0, stts0, deps0))
       | (NONE,       SOME env2') => SOME (ENV_CONSTRAINT ((emenv, env2'), labs0, stts0, deps0))
       | (NONE,       NONE)       => NONE)
  | filterOcst (ocst as IDENTIFIER_CLASS_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as FUNCTION_TYPE_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as ACCESSOR_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as LET_CONSTRAINT env) labs =
    (case filterEnv env labs of
	 SOME env' => SOME (LET_CONSTRAINT env')
       | NONE      => NONE)
  | filterOcst (ocst as SIGNATURE_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as FUNCTOR_CONSTRAINT _) labs = SOME ocst
  | filterOcst (ocst as SHARING_CONSTRAINT _) labs = SOME ocst

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
    in SOME env'
    end
  | filterEnv (env as ENVVAR _) labs = SOME env
  | filterEnv (env as SEQUENCE_ENV (env1, env2)) labs =
    (case (filterEnv env1 labs, filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (SEQUENCE_ENV (env1', env2'))
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
  | filterEnv (env as SIGNATURE_ENV (env1, env2, kind)) labs =
    (case (filterEnv env1 labs , filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (SIGNATURE_ENV (env1', env2', kind))
       | (SOME env1', NONE)       => SOME (SIGNATURE_ENV (env1', updateICmp false emenv, kind))
       | (NONE,       SOME env2') => SOME (SIGNATURE_ENV (updateICmp false emenv, env2', kind))
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as ENVPOL (tyvenv, env1)) labs =
    (case (filterIdEnv tyvenv labs , filterEnv env1 labs) of
	 ((tyvenv', _), SOME env1') => SOME (ENVPOL (tyvenv', env1'))
       | ((tyvenv', _), NONE)       =>
	 if isEmptyIdEnv tyvenv'
	 then NONE
	 else SOME (ENVPOL (tyvenv', updateICmp false emenv)))
  | filterEnv (env as DATATYPE_CONSTRUCTOR_ENV (idl as (id, lab), env1)) labs =
    (case filterEnv env1 labs of
	 SOME env1' => SOME (DATATYPE_CONSTRUCTOR_ENV (idl, env1'))
       | NONE       => if testlab lab labs
		       then SOME (DATATYPE_CONSTRUCTOR_ENV (idl, updateICmp false emenv))
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
  | filterEnv (env as FUNCTOR_ENV cst) labs =
    (case filterCst cst labs of
	 OCST ocsts => if OMC.numItems ocsts = 0
		       then NONE
		       else SOME (FUNCTOR_ENV (OCST ocsts)))
  | filterEnv (env as CONSTRAINT_ENV cst) labs =
    (case filterCst cst labs of
	 OCST ocsts => if OMC.numItems ocsts = 0
		       then NONE
		       else SOME (CONSTRAINT_ENV (OCST ocsts)))
  | filterEnv (env as ENVPTY _) _ = SOME env
  | filterEnv (env as ENVFIL (file, e, strm)) labs =
    (case (filterEnv e labs, filterEnv (strm ()) labs) of
	 (SOME env1, SOME env2) => SOME (ENVFIL (file, env1, fn () => env2))
       | (SOME env1, NONE)      => SOME (ENVFIL (file, env1, fn () => emenv))
       | (NONE,      SOME env2) => SOME (ENVFIL (file, emenv, fn () => env2))
       | (NONE,      NONE)      => NONE)
  | filterEnv (env as ENVTOP) _ = SOME env


val filterEnv = fn env => fn labs => case filterEnv env labs of NONE => updateICmp false emenv | SOME env' => env'

end
