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
 *  o File name:   StateMap.sml
 *)

(** Contains the definition of a unification state.
 * The file defines the structure StateMap which has signature STATE. *)
structure StateEnv :> STATE = struct

(* abbreviate structure names *)
structure L  = Label
structure O  = OrdSet
structure I  = Id
structure T  = Ty
structure E  = Env
structure C  = ConsId
structure D  = Debug
structure EL = ExtLab
structure CL = ClassId
structure CD = LongId
structure EH = ErrorHandler
structure MS = SplayMapFn(OrdKey)
structure MT = SplayMapFn(OrdLabLid)
(* We shouldn't use OrdLid here because OrdLid does not compare
 * the labels of the long identifiers.  This is why when 2 ids
 * with the same name are free, only one of them is reported. *)
structure SL = BinarySetFn(OrdIdl)

(** An integer list. *)
type path  = int list
(** A list of #path values. *)
type paths = path list
(** A recursive type. *)
type rcty  = T.fieldType list * T.flex * (L.label * T.fieldName) EL.extLab list

(** The state of types.. *)
type stTv = T.ty
(** The state of type functions. *)
type stTf = T.typeFunction
(** The state of equality types. *)
type stEq = (T.equalityType * T.equalityTypeVar list)
(** The state of typename.. *)
type stTn = T.typenameType
(** The state of sequences. *)
type stSq = T.rowType
(** The state of field types. *)
type stRt = T.fieldType
(** The state of label types. *)
type stLt = T.labelType
(** The state of environments. *)
type stEv = E.env
(** The state of recursive types. *)
type stRc = (rcty * rcty)
(** The state of explicit type variables. *)
type stGe = T.explicitTypeVar
(** The state of row types. *)
type stAr = T.rowType
type stOr = paths    EL.extLab
type stCl = CL.class EL.extLab
type stNa = T.typename EL.extLab

(** Compares two different type names using Int.compare *)
fun compareStNa ((tn1, _, _, _), (tn2, _, _, _)) = Int.compare (T.typenameToInt tn1, T.typenameToInt tn2)
structure NA = BinarySetFn(type ord_key = stNa val compare = compareStNa)

type 'a onestatemp = 'a MS.map ref
type 'a onestaterc = 'a list ref
type 'a onestatear = 'a MT.map ref
(*type 'a onestateub = 'a list MT.map ref*)
(* NOTE: - resp are the type variables responsible for the type variable to be mono
 *       - deps are the type variables depending on the the type varible to be mono *)
type 'a onestatege = {resp : 'a MS.map, deps : O.ordset} MS.map ref

(** The state of type variables. *)
type statetv = stTv onestatemp
(** The state of type functions. *)
type statetf = stTf onestatemp
(** The state of equality type variables. *)
type stateeq = stEq onestatemp
(** The state of type names. *)
type statetn = stTn onestatemp
(** The state of the sequence variables. *)
type statesq = stSq onestatemp
(** The state of the row variables. *)
type statert = stRt onestatemp
type statelt = stLt onestatemp
(** The state of the environment variables. *)
type stateev = stEv onestatemp
(** The state of the class variables. *)
type statecl = stCl onestatemp
(** The state of records. *)
type staterc = stRc onestaterc
type stateor = stOr onestatemp

type statege = stGe onestatege

(** Part of the state for the env *)
type stateid = Env.env ref

(** This is for the arity of type conss *)
type statear = stAr onestatear

(** This is for the free identifiers *)
type statefr = SL.set ref
(** This is for the free _opened_ identifiers *)
type statefo = SL.set ref

(** Set of type names *)
type statena = NA.set ref

(** Part of the state for the types. *)
type statese = {tv : statetv,
		tf : statetf,
		eq : stateeq, (* equality types (CHANGE THIS NAME) *)
		tn : statetn,
		sq : statesq,
		rt : statert,
		lt : statelt,
		ev : stateev,
		cl : statecl}

(** Represenst a state, a record.
 * \arg se. Unifiers.
 * \arg id. Environments.
 * \arg na. Typenames.
 * \arg rc. Records.
 * \arg ge. Monomorphism.
 * \arg or. Overloading.
 * \arg ar. Arity.
 * \arg fr. Free identifiers.
 * \arg fo. Free opened identifiers. *)
type state   = {se : statese,
		id : stateid,
		na : statena,
		rc : staterc,
		ge : statege,
		or : stateor,
		ar : statear,
		fr : statefr,
		fo : statefo}


(** Prints a list. *)
fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prints a list of integers. *)
fun printIntList xs = printlistgen xs Int.toString

(** Prints a #path value. *)
fun printPath path = printIntList path

(** Prints a #paths value. *)
fun printPaths paths = printlistgen paths printPath

fun printStateGen sta fp =
    MS.foldri (fn (k, x, y) => Int.toString k   ^ " : " ^ fp x ^ "\n"  ^ y) "" (!sta)

fun printStateGen' sta fp =
    MS.foldri (fn (k, x, y) => Int.toString k   ^ " : " ^ EL.printExtLab' x fp ^ "\n"  ^ y) "" (!sta)

(** Prints the overloading state. *)
fun printStateOr ors =
    MS.foldri (fn (k, x, y) =>
		  Int.toString k   ^ " : " ^ EL.printExtLab' x printPaths ^ "\n"  ^ y)
	      ""
	      (!ors)

(** Prints a list of pairs of labels and field names. *)
fun printlabtylist xs =
    printlistgen xs (fn ext => EL.printExtLab' ext (fn (l, lc) => "(" ^ L.printLab l ^ "," ^ T.printFieldName lc ^ ")"))

(** Prints out a record type. *)
fun printRcTy (rtl, flex, ltl) =
    "(" ^ T.printfieldtylist rtl  ^
    "," ^ T.printflex      flex ^
    "," ^ printlabtylist   ltl  ^ ")"

(** Prints out a pair of record types by calling #printRcTy. *)
fun printOneSavedRec (x1, x2) =
    "(" ^ printRcTy x1 ^ "," ^ printRcTy x2 ^ ")"

fun printStateRec srec = printlistgen (!srec) printOneSavedRec

(** Pritns an environment. *)
fun printenv x = E.printEnv x ""

(** Prints the state of identifiers. *)
fun printStateId renv = "State Id:\n"^(E.printEnv (!renv) ""  ^ "\n")

(** Prints the typename state. *)
fun printStateNa stna =
    "[" ^ #1 (NA.foldr (fn (exttn, (st, del)) =>
			   (EL.printExtLab' exttn T.printTypename ^ del ^ st, ","))
		       ("]", "")
		       (!stna))

(** Prints the state of free identifiers. *)
fun printStateFr sfr =
    "[" ^ #1 (SL.foldr (fn (idl, (st, del)) => (I.printIdL idl ^ del ^ st, ","))
		       ("]", "")
		       (!sfr))

(** Print a pair of a label and a labelled identifier. *)
fun printLabLid (lab, lid) = "(" ^ L.printLab lab ^ "," ^ I.printLid lid ^ ")"

(** Prints the arity state. *)
fun printStateAr sta =
    MT.foldri (fn (k, x, y) => printLabLid k ^ " : " ^ T.printseqty x ^ "\n"  ^ y) "" (!sta)

fun printStateResp resp =
    MS.foldri (fn (k, x, y) => Int.toString k   ^ " : " ^ EL.printExtLab' x T.printTypeVar ^ "\n"  ^ y) "" (resp)

(** Prints the dependency argument out. *)
fun printStateDeps deps = O.toString deps

fun printStateGe statege =
    MS.foldri (fn (id, {resp, deps}, st) =>
		  Int.toString id ^
		  ":{resp=" ^ printStateResp resp ^
		  ",deps="  ^ printStateDeps deps ^ "}\n" ^ st)
	      ""
	      (!statege)

fun printStateSe {tv, tf, eq, tn, sq, rt, lt, ev, cl} =
    "State TV (type variables):\n" ^ printStateGen  tv T.printty    ^ "\n" ^
    "State TF (type functions):\n" ^ printStateGen  tf T.printtyf   ^ "\n" ^
    "State EQ (equality types):\n" ^ printStateGen  eq (fn (left, right) => "(" ^ T.printEqualityType left ^ ", " ^ T.printEqualityTypeVarList right ^ ")") ^ "\n" ^
    "State TN (type names):\n" ^ printStateGen  tn T.printtnty  ^ "\n" ^
    "State SQ:\n" ^ printStateGen  sq T.printseqty ^ "\n" ^
    "State RT:\n" ^ printStateGen  rt T.printFieldType ^ "\n" ^
    "State LT:\n" ^ printStateGen  lt T.printlabty ^ "\n" ^
    "State EV (environment variables):\n" ^ printStateGen  ev printenv     ^ "\n" ^
    "State CL:\n" ^ printStateGen' cl CL.toString  ^ "\n"

(** Prints the argument. *)
fun printNames names =
    "State NA:"
    ^ printlistgen (NA.listItems (!names))
		   (fn extname => EL.printExtLab' extname T.printTypename)
    ^ "\n"

(** Prints a state to a string. Warning: A call to this function can be extremely expensive. *)
fun printState {se, id(*, ub*), na, rc, ge(*, cl*), or, ar, fr, fo} =
    printStateSe se ^
    printStateId id ^
    printNames   na ^
    (*printStateUb ub ^*)
    "State OC:\n" ^ printStateOr   or ^ "\n" ^
    (*"State CL:\n" ^ printStateGen cl E.printClass ^ "\n" ^*)
    "State RC:\n" ^ printStateRec  rc              ^ "\n" ^
    "State GE:\n" ^ printStateGe   ge              ^ "\n" ^
    "State AR:\n" ^ printStateAr   ar              ^ "\n" ^
    "State FR:\n" ^ printStateFr   fr              ^ "\n" ^
    "State FO:\n" ^ printStateFr   fo

fun getStateSe (x : state) = #se x
(** Gets the record state. *)
fun getStateRc (x : state) = #rc x
fun getStateGe (x : state) = #ge x
fun getStateOr (x : state) = #or x
fun getStateId (x : state) = #id x
fun getStateNa (x : state) = #na x
fun getStateAr (x : state) = #ar x
fun getStateFr (x : state) = #fr x
fun getStateFo (x : state) = #fo x

(** Get the type variables in the state. *)
fun getStateTv x = #tv (getStateSe x)
(** Gets the type function variables in the state. *)
fun getStateTf x = #tf (getStateSe x)
(** Gets the equality type variables in the state. *)
fun getStateEq x = #eq (getStateSe x)
(** Gets the type name variables in the state. *)
fun getStateTn x = #tn (getStateSe x)
(** Gets the sequence variables in the state. *)
fun getStateSq x = #sq (getStateSe x)
(** Gets the row type variables from the state. *)
fun getStateRt x = #rt (getStateSe x)
fun getStateLt x = #lt (getStateSe x)
(** Gets the environment variables from the state. *)
fun getStateEv x = #ev (getStateSe x)
(** Gets the class variables from the state. *)
fun getStateCl x = #cl (getStateSe x)

(** Prints the equality type part of the state. *)
fun printStateEq state =
    "State EQ:\n" ^ printStateGen  (getStateEq state) (fn (left,right) => ("(" ^ T.printEqualityType left ^ ", " ^ T.printEqualityTypeVarList right ^ ")")) ^ "\n"

(** Gets the value identifers part of a state. *)
fun getStateIdVa x = E.getValueIds x
(** Gets the explicit type variable part of a state. *)
fun getStateIdTv x = E.getExplicitTypeVars x
(** Gets the type name environment part of a state. *)
fun getStateIdTy x = E.getTypeNameEnv x
(** Gets the structures part of a state. *)
fun getStateIdSt x = E.getStructs x
(** Gets the signature part of a state. *)
fun getStateIdSi x = E.getSigs x
(** Gets the functors part of a state. *)
fun getStateIdFn x = E.getFunctors x
(** Gets the overloading class part of a state. *)
fun getStateIdOc x = E.getOverloadingClasses x
(** Gets the functors part of a state. *)
fun getStateIdFu x = E.getFunctors x

(** Calls #MS.find. *)
fun getValOneState onestate x = MS.find (!onestate, x)
(** Looks a type variable up in the state. *)
fun getValStateTv state x = getValOneState (getStateTv state) (T.typeVarToInt     x)
(** Looks a function type variable up in the state. *)
fun getValStateTf state x = getValOneState (getStateTf state) (T.typeFunctionVarToInt    x)
(** Looks an equality type variable up in the state. *)
fun getValStateEq state x = getValOneState (getStateEq state) (T.equalityTypeVarToInt    x)
(** Looks a typename variable up in the state. *)
fun getValStateTn state x = getValOneState (getStateTn state) (T.typenameVarToInt x)
(** Looks a row/sequence variable up in the state. *)
fun getValStateSq state x = getValOneState (getStateSq state) (T.rowVarToInt    x)
(** Looks a row type variable up in the state. *)
fun getValStateRt state x = getValOneState (getStateRt state) (T.fieldVarToInt    x)
(** Looks a label variable up in the state. *)
fun getValStateLt state x = getValOneState (getStateLt state) (T.labelVarToInt    x)
(** Looks a environment variable up in the state. *)
fun getValStateEv state x = getValOneState (getStateEv state) (E.envVarToInt    x)
(** Looks a class variable up in the state. *)
fun getValStateCl state x = getValOneState (getStateCl state) (CL.classvarToInt x)
(** Looks a idor variable up in the state. *)
fun getValStateOr state x = getValOneState (getStateOr state) (T.idorToInt      x
)
fun getValStateGe state x =
    let val statege = getStateGe state
	val v = T.typeVarToInt x
    in case getValOneState statege v of
	   NONE => NONE
	 | SOME {resp, deps} =>
	   SOME (MS.foldr (fn ((_, labs1, stts1, deps1), (_, labs2, stts2, deps2)) =>
			      (x, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2))
			  (x, L.empty, L.empty, CD.empty)
			  resp)
    end

(** Builds up a sequence type (repeats unifier lookup). *)
fun buildSeq state (T.ROW_VAR sv) =
    (case getValStateSq state sv of
	 NONE => T.newROW_VAR ()
       | SOME sq => buildSeq state sq)
  | buildSeq state (T.ROW_C (xs, flex, lab)) =
    T.ROW_C (map (fn _ => T.newFIELD_VAR ()) xs, flex, lab)
  | buildSeq state (T.ROW_DEPENDANCY (sq1, labs1, stts1, deps1)) =
    T.ROW_DEPENDANCY (buildSeq state sq1, labs1, stts1, deps1)

(** Bulids up an arity type (repeats unifier lookup). *)
fun buildKeyAr lid NONE = (L.dummyLab, lid)
  | buildKeyAr lid (SOME lab) = (lab, lid)

(** Given a labelled identifier looks it up in the arity state. *)
fun getValStateAr state lid labop =
    let val onestate = getStateAr state
	val key = buildKeyAr lid labop
    in case MT.find (!onestate, key) of
	   NONE => let val ext = T.newROW_VAR ()
		   in onestate := (MT.insert (!onestate, key, ext)); ext
		   end
	 | SOME sq => buildSeq state sq
    end

fun getValStateFr state = SL.listItems (!(getStateFr state))
fun getValStateFo state = SL.listItems (!(getStateFo state))

fun getValStateFree state =
    (map (fn x => (x, true))  (getValStateFo state)) @
    (map (fn x => (x, false)) (getValStateFr state))

(** Gets the domain of the type variable state. *)
fun getDomTv state =
    MS.foldri (fn (i, _, is) => O.cons i is)
	      O.empty
	      (!(getStateTv state))

fun getDomGe state =
    MS.foldri (fn (i, _, is) => O.cons i is)
	      O.empty
	      (!(getStateGe state))

fun isInGe state tv = Option.isSome (MS.find (!(getStateGe state), T.typeVarToInt tv))

(** Calls #ExtLab.unionExtLab with the arguments and the identity function. *)
fun combine x1 x2 = EL.unionExtLab x1 x2 (fn x => x)

(** Updates a state with a key and a value. *)
fun updateOneState onestate x y = onestate := (MS.insert (!onestate, x, y))

(* jpirie: replace one state here should be removed
 * updateOneState will remove the old key value pair and replace it with the new one *)
fun replaceOneState onestate x y =
    (MS.remove (!onestate, x) handle NotFound => raise EH.DeadBranch "Trying to replace a key in the equality type state but the key does not exist in the state!";
    onestate := (MS.insert (!onestate, x, y)))

(** Updates the type function part of the state. *)
fun updateStateTf state key value = updateOneState (getStateTf state) (T.typeFunctionVarToInt    key) value
(** Update the type name part of the state. *)
fun updateStateTn state key value = updateOneState (getStateTn state) (T.typenameVarToInt key) value
(** Updates the equality types part of the state. *)
fun updateStateEq state key value = updateOneState (getStateEq state) (T.equalityTypeVarToInt key) value
(** Replaces the equality types part of the state. *)
fun replaceStateEq state key value = replaceOneState (getStateEq state) (T.equalityTypeVarToInt key) value
(** Updates the sequence part of the state. *)
fun updateStateSq state key value = updateOneState (getStateSq state) (T.rowVarToInt    key) value
(** Updates the row part of the state. *)
fun updateStateRt state key value = updateOneState (getStateRt state) (T.fieldVarToInt    key) value
fun updateStateLt state key value = updateOneState (getStateLt state) (T.labelVarToInt    key) value
(** Updates the environment variables part of the state. *)
fun updateStateEv state key value = updateOneState (getStateEv state) (E.envVarToInt    key) value
fun updateStateOr state key value = updateOneState (getStateOr state) (T.idorToInt      key) value
(** Updates the class part of the state. *)
fun updateStateCl state key value = updateOneState (getStateCl state) (CL.classvarToInt key) value
fun updateStateGe state key value =
    let val statege = getStateGe state
	val v = T.typeVarToInt key
	val (tv, labs1, stts1, deps1) = value
	val u = T.typeVarToInt tv
	val _ = case getValOneState statege v of
		    NONE => let val new = {resp = MS.insert (MS.empty, u, value), deps = O.empty}
			    in statege := (MS.insert (!statege, v, new))
			    end
		  | SOME {resp, deps} =>
		    (case MS.find (resp, u) of
			 NONE => let val new = {resp = MS.insert (resp, u, value), deps = deps}
				 in statege := (MS.insert (!statege, v, new))
				 end
		       | SOME (_, labs2, stts2, deps2) =>
			 let val entry = (tv, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
			     val new = {resp = MS.insert (resp, u, entry), deps = deps}
			 in statege := (MS.insert (!statege, v, new))
			 end)
	val _ = case getValStateTv state key of
		    NONE => ()
		  | SOME ty =>
		    let val tyvars = T.getTypeVarsTy ty
		    in if List.null tyvars
		       then ()
		       else case getValStateGe state key of
				NONE => raise EH.DeadBranch "DeadBranch70"
			      | SOME (_, labs, stts, deps) =>
				let val _ = app (fn (v, labs', stts', deps') =>
						    let val labs0 = L.union  labs labs'
							val stts0 = L.union  stts stts'
							val deps0 = CD.union deps deps'
						    in updateStateGe state v (key, labs0, stts0, deps0)
						    end)
						tyvars
				    val _ = case getValOneState statege v of
						NONE => raise EH.DeadBranch "DeadBranch71"
					      | SOME {resp, deps} =>
						let val deps' = foldr (fn ((tv, _, _, _), deps) =>
									  O.cons (T.typeVarToInt tv) deps)
								      deps
								      tyvars
						    val new = {resp = resp, deps = deps'}
						in statege := (MS.insert (!statege, v, new))
					    end
				in ()
				end
		    end
    in ()
    end

(** Updates the type variable part of the state. *)
fun updateStateTv state key value =
    let val v = T.typeVarToInt key
	val _ = updateOneState (getStateTv state) v value
	val _ = case getValStateGe state key of
		    NONE => ()
		  | SOME (_, labs, stts, deps) =>
		    let val ty = value
			val tyvars = T.getTypeVarsTy ty
		    in if List.null tyvars
		       then ()
		       else let val statege = getStateGe state
				val _ = app (fn (v, labs', stts', deps') =>
						let val labs0 = L.union  labs labs'
						    val stts0 = L.union  stts stts'
						    val deps0 = CD.union deps deps'
						in updateStateGe state v (key, labs0, stts0, deps0)
						end)
					    tyvars
				val _ = case getValOneState statege v of
					    NONE => raise EH.DeadBranch "DeadBranch72"
					  | SOME {resp, deps} =>
					    let val deps' = foldr (fn ((tv, _, _, _), deps) =>
								      O.cons (T.typeVarToInt tv) deps)
								  deps
								  tyvars
						val new = {resp = resp, deps = deps'}
					    in statege := (MS.insert (!statege, v, new))
					    end
			    in ()
			    end
		    end
    in ()
    end

fun updateStateFo state lid =
    let val statefo = getStateFo state
	val statefr = getStateFr state
	val _ = statefr := SL.delete (!statefr, lid) handle LibBase.NotFound => ()
    in statefo := SL.add (!statefo, lid)
    end

fun updateStateFr state lid =
    let val statefr = getStateFr state
	val statefo = getStateFo state
    in if SL.member (!statefo, lid)
       then ()
       else statefr := SL.add (!statefr, lid)
    end

(** Tests whether a typename is in a state. *)
fun isAName tyname state =
    NA.member (!(getStateNa state), (tyname, L.empty, L.empty, CD.empty))


fun updateFoundVal NONE _ = NONE
  | updateFoundVal (SOME (bind, b1)) b2 = SOME (bind, b1 orelse b2)

fun getValStateId (env as E.ENV_CONS _) (I.ID (id, lab)) _ fenv labs stts deps =
    (case List.find (fn x => true) (E.plusproj (fenv env) id) of
	 SOME ext => (SOME (EL.updExtLab ext labs stts deps, E.getIArgOfFunctor env), NONE, true)
       | _        => (NONE, SOME ((id, lab), (env, labs, stts, deps)), false))
  | getValStateId (env as E.ENV_CONS _) (I.LID ((id, lab1), lid, lab2)) state fenv labs stts deps =
    (case List.find (fn x => true) (E.plusproj (E.getStructs env) id) of
	 NONE => (NONE, (SOME ((id, lab1), (env, labs, stts, deps))), false)
       | SOME (bind, labs', stts', deps') =>
	 let val labs0 = L.cons lab1 (L.cons lab2 (L.union labs labs'))
	     val stts0 = L.union stts stts'
	     val deps0 = CD.union deps deps'
	     val (sem, str, b) =  getValStateId (C.getBindT bind) lid state fenv labs0 stts0 deps0
	     val sem' = updateFoundVal sem (E.getIArgOfFunctor env)
	 (*val str' = if not (Option.isSome str) andalso not b
		      then SOME ((id, lab1), (env, labs0, stts0, deps0))
		      else str*)
	 (* If the returned boolean is false and that str is NONE, it means that we couldn't go deeper
	  * and so we need to update the str to some SOME something.*)
	 in (sem', str, true)
	 end)
  | getValStateId (E.ROW_ENV (env1, env2)) lid state fenv labs stts deps =
    (case getValStateId env2 lid state fenv labs stts deps of
	 (NONE, y, false) => if E.hasEnvVar env2
			     then ((*D.printdebug2 (E.printEnv env2 "");*) (NONE, y, false))
			     else getValStateId env1 lid state fenv labs stts deps
       | x => x)
  | getValStateId (E.ENV_VAR (ev, lab)) lid state fenv labs stts deps =
    (case getValStateEv state ev of
	 NONE => (NONE, NONE, false)
       | SOME env =>
	 getValStateId env
		       lid
		       state
		       fenv
		       labs
		       stts
		       deps)
  | getValStateId (E.ENVDEP (env0, labs0, stts0, deps0)) lid state fenv labs stts deps =
    getValStateId env0
		  lid
		  state
		  fenv
		  (L.union  labs labs0)
		  (L.union  stts stts0)
		  (CD.union deps deps0)
  | getValStateId env _ _ _ _ _ _ =
    (print (E.printEnv env "");
     raise EH.DeadBranch "There shouldn't be such an env in the unification env")

(* gets various fields from the record holding the env *)
fun selVa env = E.getValueIds env
(** Getsthe typename environment of the argument. *)
fun selTy env = E.getTypeNameEnv env
(** Gets the structures in the environment.*)
fun selSt env = E.getStructs env
(** Gets the signatures in the environment. *)
fun selSi env = E.getSigs env
(** Gets the overloading classes in the environment. *)
fun selOc env = E.getOverloadingClasses env

fun getValStateId' state lid fenv = getValStateId (!(getStateId state)) lid state fenv L.empty L.empty CD.empty

(** Calls getValStateId' with the appropriate argument for getState. *)
fun getValStateIdVa state lid bdown = getValStateId' state lid getStateIdVa
(** Calls getValStateId' with the appropriate argument for getState. *)
fun getValStateIdTv state lid bdown = getValStateId' state lid getStateIdTv
(** Calls getValStateId' with the appropriate argument for getState. *)
fun getValStateIdTy state lid bdown = getValStateId' state lid getStateIdTy
(** Calls getValStateId' with the appropriate argument for getState. *)
fun getValStateIdSt state lid bdown = getValStateId' state lid getStateIdSt
(** Calls getValStateId' with the appropriate argument for getState. *)
fun getValStateIdSi state lid bdown = getValStateId' state lid getStateIdSi
(** Calls getValStateId' with the appropriate argument for getState. *)
fun getValStateIdOc state lid bdown = getValStateId' state lid getStateIdOc
(** Calls getValStateId' with the appropriate argument for getState. *)
fun getValStateIdFu state lid bdown = getValStateId' state lid getStateIdFu

fun deleteStateGeDeps state key dep =
    let val statege = getStateGe state
    in case getValOneState statege key of
	   NONE => raise EH.DeadBranch "DeadBranch73"
	 | SOME {resp, deps} =>
	   let val resp' = #1 (MS.remove (resp, dep))
		   handle LibBase.NotFound => raise EH.DeadBranch "DeadBranch74"
	   in if MS.isEmpty resp'
	      then (statege := #1 (MS.remove (!statege, key));
		    O.foldr (fn (tv, _) => deleteStateGeDeps state tv key) () deps)
		   handle LibBase.NotFound => raise EH.DeadBranch "DeadBranch75"
	      else statege := (MS.insert (!statege, key, {resp = resp', deps = deps}))
	   end
    end

fun deleteStateGe state key =
    let val v = T.typeVarToInt key
    in deleteStateGeDeps state v v
    end

(** Updates a datatype constructor. in a state given an id label pair and an environment. *)
fun updateDatCons state (id, lab) (env as E.ENV_CONS _) =
    (case getValStateIdTy state (I.idToLid id lab) true of
	 (SOME (({id, bind = (bind, tnKind, cons), equalityTypeVar, lab = l, poly, class}, labs, stts, deps), _), _, _) =>
	 if L.eq lab l
	 then cons := (E.getValueIds env, E.getIComplete env)
	 else ()
       | _ => ())
  | updateDatCons state idlab env = ()

(** Tests whether a state is empty. *)
fun isEmpty state = MS.numItems (!(getStateTv state)) = 0

fun initStateId () = ref E.emptyEnv

(** Initialises a state. *)
fun initStateSe () =
    let val atv = ref MS.empty
	val atf = ref MS.empty
	val aeq = ref MS.empty (* equality types (CHANGE THIS NAME) *)
	val atn = ref MS.empty
	val asq = ref MS.empty
	val art = ref MS.empty
	val alt = ref MS.empty
	val aev = ref MS.empty
	val acl = ref MS.empty
    in {tv = atv,
	tf = atf,
	eq = aeq,
	tn = atn,
	sq = asq,
	rt = art,
	lt = alt,
	ev = aev,
	cl = acl}
    end

(** Initialize the state with empty sets. *)
fun initState () =
    let val ase = initStateSe ()
	val aid = initStateId ()
	val arc = ref []
	val ana = ref NA.empty
	val age = ref MS.empty
	val aor = ref MS.empty
	val aar = ref MT.empty
	val afr = ref SL.empty
	val afo = ref SL.empty
    in {se = ase,
	id = aid,
	na = ana,
	rc = arc,
	ge = age,
	or = aor,
	ar = aar,
	fr = afr,
	fo = afo}
    end

(* TO HANDLE RECORDS *)


(**************************************************************)
(* we need a function to check if a record is complete        *)
(* we need to introduce staterv and state lv                  *)
(* we need a function to update srec with staterv and statelv *)
(* - move all these stuff into State                          *)
(* - or bettter move that in a new structure                  *)
(*   and include that in State*)

fun reportLabTyClashLC (T.LABEL_VAR _) = NONE
  | reportLabTyClashLC (T.LC (lc, lab)) =
    SOME (L.toInt lab, lc, L.empty, L.empty, CD.empty)
  | reportLabTyClashLC (T.LABEL_DEPENDANCY (lt, labs, stts, deps)) =
    (case reportLabTyClashLC lt of
	 SOME (lab, lc, labs0, stts0, deps0) =>
	 let val labs1 = L.union  labs labs0
	     val stts1 = L.union  stts stts0
	     val deps1 = CD.union deps deps0
	 in SOME (lab, lc, labs1, stts1, deps1)
	 end
       | NONE => NONE)

fun reportLabTyClashRC (T.FIELD_VAR _) = NONE
  | reportLabTyClashRC (T.FC (lt, _, _)) = reportLabTyClashLC lt
  | reportLabTyClashRC (T.FIELD_DEPENDANCY (field, labs, stts, deps)) =
    (case reportLabTyClashRC field of
	 SOME (lab, lc, labs0, stts0, deps0) =>
	 let val labs1 = L.union  labs labs0
	     val stts1 = L.union  stts stts0
	     val deps1 = CD.union deps deps0
	 in SOME (lab, lc, labs1, stts1, deps1)
	 end
       | NONE => NONE)
  | reportLabTyClashRC T.FIELD_NO_OVERLOAD = NONE

fun reportLabTyClash fields =
    foldr (fn (field, (list, labs, stts, deps)) =>
	      case reportLabTyClashRC field of
		  SOME (lab, lc, labs0, stts0, deps0) =>
		  let val labs1 = L.union  labs labs0
		      val stts1 = L.union  stts stts0
		      val deps1 = CD.union deps deps0
		  in ((lab, lc) :: list, labs1, stts1, deps1)
		  end
		| NONE => (list, labs, stts, deps))
	  ([], L.empty, L.empty, CD.empty)
	  fields

fun reportRcty xs =
    foldr (fn (((lab, lc), labs1, stts1, deps1), (list, labs2, stts2, deps2)) =>
	      let val labs0 = L.union  labs1 labs2
		  val stts0 = L.union  stts1 stts2
		  val deps0 = CD.union deps1 deps2
	      in ((L.toInt lab, lc) :: list, labs0, stts0, deps0)
	      end)
	  ([], L.empty, L.empty, CD.empty)
	  xs

fun interEmpty rtl1 rtl2 =
    let fun gatherLC (T.LC (lc, _)) = [lc]
	  | gatherLC (T.LABEL_DEPENDANCY elt)     = gatherLC (EL.getExtLabT elt)
	  | gatherLC (T.LABEL_VAR _)       = []
	fun gatherRC []                        = []
	  | gatherRC ((T.FC (lt, _, _)) :: xs) = (gatherLC lt) @ (gatherRC xs)
	  | gatherRC ((T.FIELD_DEPENDANCY efield) :: xs)       = gatherRC ((EL.getExtLabT efield) :: xs)
	  | gatherRC ((T.FIELD_VAR _) :: xs)          = gatherRC xs
	  | gatherRC (T.FIELD_NO_OVERLOAD :: xs)              = gatherRC xs
    in List.null (Tools.inter (gatherRC rtl1) (gatherRC rtl2))
    end

fun isnotEmptyLC (T.LC _)   = true
  | isnotEmptyLC (T.LABEL_DEPENDANCY elt) = isnotEmptyLC (EL.getExtLabT elt)
  | isnotEmptyLC (T.LABEL_VAR _)   = false

fun isnotEmpty []                         = false
  | isnotEmpty ((T.FC (lt, _, _)) :: rtl) = isnotEmptyLC lt orelse isnotEmpty rtl
  | isnotEmpty ((T.FIELD_DEPENDANCY efield) :: rtl)       = isnotEmpty ((EL.getExtLabT efield) :: rtl)
  | isnotEmpty ((T.FIELD_VAR _) :: rtl)          = isnotEmpty rtl
  | isnotEmpty (T.FIELD_NO_OVERLOAD :: rtl)              = isnotEmpty rtl

fun isCompleteLC (T.LC _)   = true
  | isCompleteLC (T.LABEL_DEPENDANCY elt) = isCompleteLC (EL.getExtLabT elt)
  | isCompleteLC (T.LABEL_VAR _)   = false

fun isComplete []                         = true
  | isComplete ((T.FC (lt, _, _)) :: rtl) = isCompleteLC lt andalso isComplete rtl
  | isComplete ((T.FIELD_DEPENDANCY efield) :: rtl)       = isComplete ((EL.getExtLabT efield) :: rtl)
  | isComplete ((T.FIELD_VAR _) :: rtl)          = false
  | isComplete (T.FIELD_NO_OVERLOAD :: rtl)              = false

fun getFieldNameLC lc0 (T.LC (lc, lab)) =
    if lc0 = lc
    then SOME (lab, lc, L.empty, L.empty, CD.empty)
    else NONE
  | getFieldNameLC lc0 (T.LABEL_DEPENDANCY (lt, labs, stts, deps)) =
    (case getFieldNameLC lc0 lt of
	 SOME (lab, lc, labs0, stts0, deps0) =>
	 let val labs1 = L.union  labs labs0
	     val stts1 = L.union  stts stts0
	     val deps1 = CD.union deps deps0
	 in SOME (lab, lc, labs1, stts1, deps1)
	 end
       | NONE => NONE)
  | getFieldNameLC lc0 (T.LABEL_VAR _) = NONE

fun getFieldNameRC lc (rt as T.FC (lt, ty, lab)) =
    (case getFieldNameLC lc lt of
	 SOME (lab0, lc0, labs0, stts0, deps0) =>
	 SOME (ty, lab0, lc0, labs0, stts0, deps0)
       | NONE => NONE)
  | getFieldNameRC lc (T.FIELD_DEPENDANCY (field, labs, stts, deps)) =
    (case getFieldNameRC lc field of
	 SOME (ty, lab, lc, labs0, stts0, deps0) =>
	 let val labs1 = L.union  labs labs0
	     val stts1 = L.union  stts stts0
	     val deps1 = CD.union deps deps0
	 in SOME (ty, lab, lc, labs1, stts1, deps1)
	 end
       | NONE => NONE)
  | getFieldNameRC lc (T.FIELD_VAR _) = NONE
  | getFieldNameRC lc T.FIELD_NO_OVERLOAD = NONE

fun getFieldName lc [] = NONE
  | getFieldName lc (field :: fields) =
    (case getFieldNameRC lc field of
	 SOME (ty, lab, lc, labs, stts, deps) =>
	 SOME (ty, lab, lc, labs, stts, deps, fields)
       | NONE =>
	 (case getFieldName lc fields of
	      SOME (ty, lab, lc, labs, stts, deps, fields') =>
	      SOME (ty, lab, lc, labs, stts, deps, field :: fields')
	    | NONE => NONE))

(** We remove the matching pairs of fieldtys and create constraints from them.
 * We don't want to loose the information for the matching pairs however.
 * - so we store them in llc1 and llc2 *)
fun ziprecLC (T.LC (lc, lab)) rtl =
    (case getFieldName lc rtl of
	 SOME (ty, lab0, lc0, labs0, stts0, deps0, rtl) =>
	 SOME (ty, lab0, lc0, lab, lc, labs0, stts0, deps0, rtl)
       | NONE => NONE)
  | ziprecLC (T.LABEL_DEPENDANCY (lt, labs, stts, deps)) rtl =
    (case ziprecLC lt rtl of
	 SOME (ty, lab1, lc1, lab2, lc2, labs', stts', deps', rtl) =>
	 let val labs0 = L.union  labs labs'
	     val stts0 = L.union  stts stts'
	     val deps0 = CD.union deps deps'
	 in SOME (ty, lab1, lc1, lab2, lc2, labs0, stts0, deps0, rtl)
	 end
       | NONE => NONE)
  | ziprecLC (T.LABEL_VAR _) _ = NONE

fun ziprecRC (T.FC (lt, ty, _)) rtl =
    (case ziprecLC lt rtl of
	 SOME (ty', lab1, lc1, lab2, lc2, labs, stts, deps, rtl') =>
	 SOME (ty, ty', lab1, lc1, lab2, lc2, labs, stts, deps, rtl')
       | NONE => NONE)
  | ziprecRC (T.FIELD_DEPENDANCY (field, labs, stts, deps)) rtl =
    (case ziprecRC field rtl of
	 SOME (ty1, ty2, lab1, lc1, lab2, lc2, labs', stts', deps', rtl') =>
	 let val labs0 = L.union  labs labs'
	     val stts0 = L.union  stts stts'
	     val deps0 = CD.union deps deps'
	 in SOME (ty1, ty2, lab1, lc1, lab2, lc2, labs0, stts0, deps0, rtl')
	 end
       | NONE => NONE)
  | ziprecRC (T.FIELD_VAR _) _ = NONE
  | ziprecRC T.FIELD_NO_OVERLOAD _ = NONE

fun ziprec [] rtl2 = ([], [], rtl2, [], [])
  | ziprec (rt :: rtl1) rtl2 =
    (case ziprecRC rt rtl2 of
	 SOME (ty1, ty2, lab1, lc1, lab2, lc2, labs, stts, deps, rtl2') =>
	 let val (cs, rtl3, rtl4, llc1, llc2) = ziprec rtl1 rtl2'
	     val c = E.genCstTyAll ty1 ty2 labs stts deps
	     val rcty1 = ((lab1, lc1), labs, stts, deps)
	     val rcty2 = ((lab2, lc2), labs, stts, deps)
	 in (c :: cs, rtl3, rtl4, rcty1 :: llc1, rcty2 :: llc2)
	 end
       | NONE =>
	 let val (cs, rtl3, rtl4, llc1, llc2) = ziprec rtl1 rtl2
	 in (cs, rt :: rtl3, rtl4, llc1, llc2)
	 end)

fun ftestrecord (srec as ((rtl1, b1, llc1), (rtl2, b2, llc2))) =
    let val (cs, rtl1', rtl2', llc1', llc2') = ziprec rtl1 rtl2
	val llc11 = llc1 @ llc1'
	val llc22 = llc2 @ llc2'
    in case (rtl1', rtl2') of
	   ([], []) => ([], cs, [])
	 | _        =>
	   if isComplete rtl1'
	      andalso isComplete rtl2' (* NEW *)
	      andalso isnotEmpty rtl2'
	      andalso interEmpty rtl2' rtl1'
	      andalso not (T.isflex b1)
	   (* they don't match *)
	   (* don't we want to check when both of them are complete? - see 65 (second line) *)
	   then let val (list1, labs1, stts1, deps1) = reportLabTyClash rtl1'
		    val (list2, labs2, stts2, deps2) = reportLabTyClash rtl2'
		    val (list3, labs3, stts3, deps3) = reportRcty llc11
		    val (list4, labs4, stts4, deps4) = reportRcty llc22
		    val labs0 = L.union  labs1 (L.union  labs2 (L.union  labs3 labs4))
		    val stts0 = L.union  stts1 (L.union  stts2 (L.union  stts3 stts4))
		    val deps0 = CD.union deps1 (CD.union deps2 (CD.union deps3 deps4))
		in ([], [], [((list1, list2, list3, list4), labs0, stts0, deps0)])
		end
	   else if isComplete rtl2'
		   andalso isComplete rtl1' (* NEW *)
		   andalso isnotEmpty rtl1'
		   andalso interEmpty rtl1' rtl2'
		   andalso not (T.isflex b2)
	   then let val (list1, labs1, stts1, deps1) = reportLabTyClash rtl1'
		    val (list2, labs2, stts2, deps2) = reportLabTyClash rtl2'
		    val (list3, labs3, stts3, deps3) = reportRcty llc11
		    val (list4, labs4, stts4, deps4) = reportRcty llc22
		    val labs0 = L.union  labs1 (L.union  labs2 (L.union  labs3 labs4))
		    val stts0 = L.union  stts1 (L.union  stts2 (L.union  stts3 stts4))
		    val deps0 = CD.union deps1 (CD.union deps2 (CD.union deps3 deps4))
		in ([], [], [((list1, list2, list3, list4), labs0, stts0, deps0)])
		end
	   else if (List.null rtl1' andalso T.isflex b1)
		   orelse
		   (List.null rtl2' andalso T.isflex b2)
	   then ([], cs, [])
	   else ([((rtl1', b1, llc11), (rtl2', b2, llc22))], cs, [])
    end

fun ftestrecords srecs =
    (fn (srs, cs, err) => (srecs := srs; (cs, err)))
	(foldr (fn (srec, (srs, cs, err)) =>
		   (fn (x, y, z) => (x @ srs, y @ cs, z @ err))
		       (ftestrecord srec))
	       ([], [], [])
	       (!srecs))

(** Sts is for status and cds us for context dependencies. *)
fun updateRecordRt state ((rtl1, b1, llc1), (rtl2, b2, llc2)) =
    let (** Helper function for #updateRecordRt. *)
	fun updateRC (field as T.FIELD_VAR rv) =
	    (case getValStateRt state rv of
		 NONE => field
	       | SOME field => field)
	  | updateRC (T.FIELD_DEPENDANCY efield) = T.FIELD_DEPENDANCY (EL.mapExtLab efield updateRC)
	  | updateRC (field as T.FC _) = field
	  | updateRC (field as T.FIELD_NO_OVERLOAD) = field
	fun update fields = map updateRC fields
    in ((update rtl1, b1, llc1), (update rtl2, b2, llc2))
    end

(** Maps #updateRecordRt over the list argument. *)
fun updateRecordsRt state srecs =
    srecs := (map (fn x => updateRecordRt state x) (!srecs))

fun updateRecordLt state ((rtl1, b1, llc1), (rtl2, b2, llc2)) =
    let fun updateLC (lt as T.LABEL_VAR lv) =
	    (case getValStateLt state lv of
		 SOME lt => lt
	       | NONE => lt)
	  | updateLC (T.LABEL_DEPENDANCY elt) = T.LABEL_DEPENDANCY (EL.mapExtLab elt updateLC)
	  | updateLC (lt as T.LC _) = lt
	fun updateRC (T.FC (lt, tv, lab)) = T.FC (updateLC lt, tv, lab)
	  | updateRC (T.FIELD_DEPENDANCY efield) = T.FIELD_DEPENDANCY (EL.mapExtLab efield updateRC)
	  | updateRC (field as T.FIELD_VAR _) = field
	  | updateRC (field as T.FIELD_NO_OVERLOAD) = field
	fun update fields = map updateRC fields
    in ((update rtl1, b1, llc1), (update rtl2, b2, llc2))
    end

fun updateRecordsLt state srecs =
    srecs := (map (fn x => updateRecordLt state x) (!srecs))

fun updateRec state =
    let val srec = getStateRc state
	val _ = updateRecordsRt state srec
	val _ = updateRecordsLt state srec
    in ftestrecords srec
    end

(** sr3 is not always empty.  Sometimes we create incomplete records.
 * We don't really need to memorise them because if a record is incomplete
 * at some point then it will always be incomplete.  This comes from the
 * fact that the fields of a record are always treated before the record
 * itself (because of the way we label a record - smaller labels for the
 * fields - and because of the order in which the labels are handled during
 * unification - smaller labels first).
 * The main problem being that we do that for records AND tuples because
 * they share the same structure.  We might actually end up having a huge
 * record state. *)
fun updateRecOne state strc =
    let	val sr1 = updateRecordRt state strc
	val sr2 = updateRecordLt state sr1
	val (sr3, cs, err) = ftestrecord sr2
	val srec = getStateRc state
	val _ = srec := sr3 @ (!srec)
    in (cs, err)
    end

(** Gets all the typenames from an environment. *)
fun getAllTns (env as E.ENV_CONS _) state =
    E.foldrienv (fn (_, sem, tns) => foldr (fn (bind, tns) => (getAllTns (E.getBindT bind) state) @ tns)
					   tns
					   sem)
		(E.getITypeNames env)
		(E.getStructs env)
  | getAllTns (E.ROW_ENV (env1, env2)) state = (getAllTns env1 state) @ (getAllTns env2 state)
  | getAllTns (E.ENV_VAR (ev, lab)) state =
    (case getValStateEv state ev of (* We need that because we don't fully build up structures as we should! *)
	 NONE => []
       | SOME env => getAllTns env state)
  | getAllTns (E.ENVDEP extenv) state = getAllTns (EL.getExtLabT extenv) state
  | getAllTns _ _ = raise EH.DeadBranch "DeadBranch76"

(** Combines type variables. *)
fun combineTyVars monos1 monos2 =
    MS.unionWith (fn ((tv1, labs1, stts1, deps1), (tv2, labs2, stts2, deps2)) =>
		     (tv1,
		      L.union  labs1 labs2,
		      L.union  stts1 stts2,
		      CD.union deps1 deps2))
		 (monos1, monos2)

(** The empty set of monomorphic variables .*)
val emMonos = MS.empty

(** Gets the monomorphic type variables in the environment given in the argument. *)
fun getMonoTyVars (env as E.ENV_CONS _) state =
    let val vids = E.getValueIds env
	val strs = E.getStructs env
	(*val _ = D.printdebug2 (E.printEnv env "")*)
	(*val _ = D.printdebug2 (E.printEnv env "")*)
    in E.foldrienv (fn (_, binds, monos) =>
		       foldr (fn (bind, monos) =>
				 if E.isMonoBind bind
				 then (*N*)let val labs = EL.getExtLabL bind
					       val stts = EL.getExtLabE bind
					       val deps = EL.getExtLabD bind
					       val tvs  = C.getTypeVars (EL.getExtLabT bind)
					       val monos' = foldr (fn ((tv, labs', stts', deps'), tvs) =>
								      if isInGe state tv
								      then tvs
								      else let val labs0 = L.union  labs labs'
									       val stts0 = L.union  stts stts'
									       val deps0 = CD.union deps deps'
									   in MS.insert (tvs, T.typeVarToInt tv, (tv, labs0, stts0, deps0))
									   end)
								  MS.empty
								  tvs
					   in combineTyVars monos' monos
					   end
				 else monos)
			     monos
			     binds)
		   (getMonoTyVarsStrEnv strs state)
		   vids
    end
  | getMonoTyVars (E.ROW_ENV (env1, env2)) state =
    let val monos = getMonoTyVars env2 state
    in if E.isENV_VAR env2
       then monos
       else combineTyVars monos (getMonoTyVars env1 state)
    end
  | getMonoTyVars (E.ENV_VAR (ev, lab)) state =
    (case getValStateEv state ev of (* We need that because we don't fully build up structures as we should! *)
	 NONE => emMonos
       | SOME env => getMonoTyVars env state)
  | getMonoTyVars (E.ENVDEP extenv) state = getMonoTyVars (EL.getExtLabT extenv) state
  | getMonoTyVars _ _ = raise EH.DeadBranch "DeadBranch77"

(** Get monomorphic type variables from a structure environment. *)
and getMonoTyVarsStrEnv strenv state =
    E.foldrienv (fn (_, binds, monos) =>
		    foldr (fn (bind, monos) =>
			      let val env  = E.getBindT bind
				  (*val _ = D.printdebug2 (E.printEnv env "")*)
				  val labs = EL.getExtLabL bind
				  val stts = EL.getExtLabE bind
				  val deps = EL.getExtLabD bind
				  val monos' = getMonoTyVars env state
			      in combineTyVars monos' monos
			      end)
			  monos
			  binds)
		emMonos
		strenv


(** When pusing an env to a state we have to also push its type names. *)
fun pushEnvToState bempty env state =
    if bempty
    then ([], [])
    else let (*N*)val tyvars  = MS.foldr (fn (v as (tv, _, _, _), tyvars) => (updateStateGe state tv v; tv :: tyvars))
					 []
					 (getMonoTyVars env state)
	     (*val tyvars  = map (fn v as (tv, _, _, _) => (updateStateGe state tv v; tv))
				 (getMonoTyVars env state)*)
	     (*val _ = D.printdebug2 (T.printTypeVarlist tyvars)*)
	     val tns     = map (fn {id, lab, kind, name} => (name, L.empty, L.empty, CD.empty)) (getAllTns env state)
	     val statena = getStateNa state
	     val _       = statena := NA.addList (!statena, tns)
	     val stateid = getStateId state
	     val _       = stateid := E.ROW_ENV (!stateid, env)
	 in (tyvars, tns)
	 end


(** When removing an env from a state we also have to remove its type names. *)
fun remEnvFromState bempty (tyvars, tns) state =
    if bempty
    then ()
    else let val _       = app (fn tv => deleteStateGe state tv) tyvars
	     val statena = getStateNa state
	     (* This is not going to work because of sharing: *)
	     val _       = app (fn tn => statena := NA.delete (!statena, tn) handle LibBase.NotFound => ()) tns
	     val stateid = getStateId state
	 in case !stateid of
		E.ROW_ENV (env1, env2) => stateid := env1
	      | _ => raise EH.DeadBranch "It appears that the unification env is not a row"
	 end


(** Checks if the env in the state hids with env vars. *)
fun hasEnvVar state = E.hasEnvVar (!(getStateId state))

end

