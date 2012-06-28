(* Copyright 2009 2010 2012 Heriot-Watt University
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
 *  o File name:   State.sig
 *  o Description: Defines the signature STATE for a unification state.
 *)


signature STATE = sig

    type path  = int list (* the int corresponds to the nth type of a ortype *)
    type paths = path list
    type rcty  = Ty.fieldType list * Ty.flex * (Label.label * Ty.fieldName) ExtLab.extLab list

    type stTv = Ty.ty
    type stTf = Ty.typeFunction
    type stEq = Ty.equalityType
    type stTn = Ty.typenameType
    type stSq = Ty.rowType
    type stRt = Ty.fieldType
    type stLt = Ty.labelType
    type stEv = Env.env
    type stRc = (rcty * rcty) (* this is for records *)
    type stGe = Ty.explicitTypeVar
    type stAr = Ty.rowType
    type stOr = paths         ExtLab.extLab
    type stCl = ClassId.class ExtLab.extLab
    type stNa = Ty.typename     ExtLab.extLab
    (* We could also add, along with the typeVar list, a boolean to mark
     * such an entry and dependent (true) or independent (false).
     * We might need the depth of the dependency as well (rank?). *)

    (*(* stUb: is among other things for the arity of type names. *)
    (* (2010-02-17) We really want an env for free identifiers with stateub *)
    type stUb = Env.class ExtLab.extLab*)

    (* Unification env *)
    type state

    (* ACCESS THE UNIFIERS AND ENVS *)
    val getValStateTv      : state -> Ty.typeVar         -> stTv option
    val getValStateTf      : state -> Ty.typeFunctionVar        -> stTf option
    val getValStateEq      : state -> Ty.equalityTypeVar        -> stEq option
    val getValStateTn      : state -> Ty.typenameVar     -> stTn option
    val getValStateSq      : state -> Ty.rowVar        -> stSq option
    val getValStateRt      : state -> Ty.fieldVar        -> stRt option
    val getValStateLt      : state -> Ty.labelVar        -> stLt option
    val getValStateEv      : state -> Env.envVar       -> stEv option
    val getValStateOr      : state -> Ty.idor          -> stOr option
    val getValStateCl      : state -> ClassId.classvar -> stCl option
    val getValStateGe      : state -> Ty.typeVar         -> stGe option

    val getValStateAr      : state -> Id.lid -> Label.label option -> stAr

    (* The second returned value is for incomplete structures.
     * The third  returned value is true if we successfully went down the lid.
     * In the first option, the bool is true if the binding comes from the parameter of a functor. *)
    (* idva is IdValue, because it's an an value pair. change this name! *)
    val getValStateIdVa    : state -> Id.lid -> bool -> (Env.extVar * bool) option * (Id.labelledId * Env.env ExtLab.extLab) option * bool
    val getValStateIdTv    : state -> Id.lid -> bool -> (Env.explicitTypeVar * bool) option * (Id.labelledId * Env.env ExtLab.extLab) option * bool
    val getValStateIdTy    : state -> Id.lid -> bool -> (Env.extType * bool) option * (Id.labelledId * Env.env ExtLab.extLab) option * bool
    val getValStateIdSt    : state -> Id.lid -> bool -> (Env.extstr * bool) option * (Id.labelledId * Env.env ExtLab.extLab) option * bool
    val getValStateIdSi    : state -> Id.lid -> bool -> (Env.extsig * bool) option * (Id.labelledId * Env.env ExtLab.extLab) option * bool
    val getValStateIdOc    : state -> Id.lid -> bool -> (Env.extovc * bool) option * (Id.labelledId * Env.env ExtLab.extLab) option * bool
    val getValStateIdFu    : state -> Id.lid -> bool -> (Env.extfun * bool) option * (Id.labelledId * Env.env ExtLab.extLab) option * bool

    (* get the 'fr' and 'op' parts of the state. *)
    val getValStateFree    : state -> (Id.labelledId * bool) list


    (*val getValStateApFirst : state -> Id.lid -> stUb option*)

    (* UPDATE THE UNIFIERS AND ENVS *)
    val updateStateTv      : state -> Ty.typeVar         -> stTv -> unit
    val updateStateTf      : state -> Ty.typeFunctionVar        -> stTf -> unit
    val updateStateEq      : state -> Ty.equalityTypeVar        -> stEq -> unit
    val replaceStateEq     : state -> Ty.equalityTypeVar        -> stEq -> unit
    val updateStateTn      : state -> Ty.typenameVar     -> stTn -> unit
    val updateStateSq      : state -> Ty.rowVar        -> stSq -> unit
    val updateStateRt      : state -> Ty.fieldVar        -> stRt -> unit
    val updateStateLt      : state -> Ty.labelVar        -> stLt -> unit
    val updateStateEv      : state -> Env.envVar       -> stEv -> unit
    val updateStateOr      : state -> Ty.idor          -> stOr -> unit
    val updateStateCl      : state -> ClassId.classvar -> stCl -> unit
    val updateStateGe      : state -> Ty.typeVar         -> stGe -> unit

    (* for free identifiers *)
    val updateStateFr      : state -> Id.labelledId -> unit
    (* for free _opened_ identifiers *)
    val updateStateFo      : state -> Id.labelledId -> unit

    val updateDatCons      : state -> Id.labelledId -> Env.env -> unit

    val updateRecOne       : state ->
			     stRc  ->
			     Env.oneConstraint list *
			     (ErrorKind.recerr ExtLab.extLab) list
    val updateRec          : state ->
			     Env.oneConstraint list *
			     (ErrorKind.recerr ExtLab.extLab) list

    (*val updateStateTc      : state -> Id.lid -> stUb -> Env.oneConstraint list
     val updateStateAp      : state -> Id.lid -> stUb -> Env.oneConstraint list*)

    (* eraseStateGe is similar to deleteStateGe but it completely removes
     * the entry for a type variable (int) when deleteStateGe only removes
     * the information in stGe.  eraseStateGe is used for internal type
     * variables when deleteStateGe is used for explicit type variables. *)
    (*val deleteStateGe      : state -> int -> unit*)
    (*val eraseStateGe       : state -> int -> unit*)

    (* GLOBAL HANDLING OF UNIFIERS AND ENVS *)
    val getDomTv           : state -> OrdSet.ordset
    val getDomGe           : state -> OrdSet.ordset

    val isInGe             : state -> Ty.typeVar -> bool

    val initState          : unit  -> state
    (*val resetState         : state -> unit*)
    val isEmpty            : state -> bool
    (*val copyState          : state -> state*)

    (* checks if the typename is in the state *)
    val isAName            : Ty.typename -> state -> bool

    (* PUSH AN ENV ONTO A STATE *)
    val pushEnvToState     : bool -> Env.env -> state -> (Ty.typeVar list * stNa list)

    (* REMOVE AN ENV FROM A STATE *)
    val remEnvFromState    : bool -> (Ty.typeVar list * stNa list) -> state -> unit

    (* CHECKS IF THE ENV IN THE STATE HIDS WITH ENV VARIABLES *)
    val hasEnvVar          : state -> bool

    (* PRINT THE UNIFIERS AND ENVS *)
    val printState         : state -> string

    (*val combineStates      : state -> state -> Env.csext list*)
    (*val generalise         : state -> Label.ordset -> unit*)

    (*(* The first boolean is true for the opening of a structure and false for a datatype replication. *)
    (* The second boolean is false if we want to open everything (incomplete structure for example).  *)
    val updateStateOpn     : state          ->
			     Id.lid         ->
			     Label.label   ->
			     ClassId.strenv ->
			     Env.opnkind    ->
			     bool           ->
			     ErrorKind.unmerr ExtLab.extLab option*)

    (*(* The boolean arguments are explained in updateStateOpn. *)
    val deleteStateOpn     : state          ->
			     Id.lid         ->
			     ClassId.strenv ->
			     Env.opnkind    ->
			     bool           ->
			     unit*)

    (*val updateStateIdVa    : state -> Id.id -> Env.extty  -> unit
     val updateStateIdTy    : state -> Id.id -> Env.extty  -> unit
     val updateStateIdSt    : state -> Id.id -> Env.extenv -> unit
     val updateStateIdSi    : state -> Id.id -> Env.extenv -> unit
     val updateStateIdOc    : state -> Id.id -> Env.extseq -> unit*)

    (* REMOVE FROM THE ENVS *)
    (*val deleteStateIdVa    : state -> Id.id -> unit
    val deleteStateIdTy    : state -> Id.id -> unit
    val deleteStateIdSt    : state -> Id.id -> unit
    val deleteStateIdSi    : state -> Id.id -> unit
    val deleteStateIdOc    : state -> Id.id -> unit*)

end
