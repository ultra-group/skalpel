(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   AstSML.sig
 *)

(** Signature for refstruct{AstSML}. *)
signature ASTSML = sig

    type next = Label.label

    type file = string (* file name *)
    type bas  = bool   (* true if it is a basis file *)

    datatype progs =
	     Progs      of (prog * file * bas * next) list

    and prog =
	Prog            of progone list
      | ProgDots        of part list

    and afile =
	AFile           of file * Reg.region * Label.label * next (* This has to be like open *)
      | AFileDots

    and abool =
	ABool           of string * Reg.region * Label.label * next
      | ABoolDots

    and progone =
	ProgOneDec      of topdec
      | ProgOneExp      of exp * Id.id * Reg.region * Label.label * next
      | ProgOneParse    of string * Reg.region list * Label.label * next (* This is for a parsing error *)
      | ProgOneFile     of afile * next (* This this for a tes files *)
      | ProgOneDots     of part list

    and topdec =
	TopDec          of topdecone list
      | TopDecDots      of part list

    and topdecone =
	TopDecOneTes    of smltes  * next
      | TopDecOneDec    of atopdec * next
      | TopDecOneDots   of part list

    and smltes =
	SmlTesDec       of atopdec * Reg.region list * next
      | SmlTesSpec      of spec    * Reg.region list * next
      | SmlTesUse       of afile   * Reg.region list * next
      | SmlTesSBas      of afile   * Reg.region list * next     (* Set Basis *)
      | SmlTesCBas      of Reg.region list * Label.label * next (* Clear Basis - Why a label? *)
      | SmlTesQuote     of abool   * Reg.region list * next
      | SmlTesType      of string  * Reg.region list * next
      | SmlTesDots      of part list

    and atopdec =
	ATopDecStr      of strdec
      | ATopDecSig      of sigdec
      (*| TopDecOneFun    of fundec*) (* This has been moved in strdecone to comply with SML/NJ syntax *)
      | ATopDecDots     of part list

    (*and fundec =
	FunDec          of funbind * Reg.region * Label.label * next
      | FunDecDots      of part list*)

    and funbind =
	FunBind         of funbindone list * Reg.region list * next
      | FunBindDots     of part list

    and funbindone =
	FunBindO        of funid * strid * labsigexp * labstrexp * Reg.region list * Label.label * next
      | FunBindOO       of funid * strid * labsigexp * labsigexp * labstrexp * Reg.region list * Label.label * next (* Opaque *)
      | FunBindOT       of funid * strid * labsigexp * labsigexp * labstrexp * Reg.region list * Label.label * next (* Translucide *)
      | FunBindOS       of funid * spec  * labstrexp * Reg.region list * Label.label * next                   (* with specs *)
      | FunBindOSO      of funid * spec  * labsigexp * labstrexp * Reg.region list * Label.label * next          (* with Specs - Opaque *)
      | FunBindOST      of funid * spec  * labsigexp * labstrexp * Reg.region list * Label.label * next          (* with Specs - Translucide *)
      | FunBindODots    of part list

    and sigdec =
	SigDec          of sigbind * Reg.region * next
      | SigDecDots      of part list

    and sigbindone =
	SigBindOne      of sigid * labsigexp * Reg.region * Label.label * next
      | SigBindOneDots  of part list

    and sigbind =
	SigBind         of sigbindone list * Reg.region list * next
      | SigBindDots     of part list

    and strdec =
	StrDec          of strdecone list * Label.label * next
      (* the regions are for the white spaces between strdecones *)
      (* we really want that in StrExpBasic and SigExpBasic *)
      | StrDecDots      of part list

    and strdecone =
	StrDecOneDec    of decs
      | StrDecOneStr    of strbind * Reg.region * next
      | StrDecOneLoc    of strdec * strdec * Reg.region list * Label.label * next
      | StrDecOneFun    of funbind * Reg.region * Label.label * next (* This was a fundec before, used in topdecone - Why do we need the label for? *)
      | StrDecOneDots   of part list

    and strbind =
	StrBind         of strbindone list * Reg.region list * next
      | StrBindDots     of part list

    and strbindone =
	StrBindOneOp    of strid * labsigexp * labstrexp * Reg.region list * Label.label * next (* derived opaque form *)
      |	StrBindOneTr    of strid * labsigexp * labstrexp * Reg.region list * Label.label * next (* derived transparent form *)
      | StrBindOne      of strid * labstrexp * Reg.region * Label.label * next
      | StrBindOneDots  of part list

    and ltreadescone = (* stands for long type realisation desctiption one *)
	LTReaDOne       of ldatname * labtype * Reg.region list * Label.label * next
      | LTReaDOneDots   of part list

    and ltreadesc =
	LTReaDesc       of ltreadescone list * Reg.region list * Label.label * next
      | LTReaDescDots   of part list

    (*and treadescone = (* WE can remove this one - it is the same as the tdrdesc *)
	TReaDOne        of datname * labtype * Reg.region * Label.label * next
      | TReaDOneDots    of part list

    and treadesc =
	TReaDesc        of treadescone list * Reg.region list * next
      | TReaDescDots    of part list*)

    and labsigexp =
	LabSigExp       of sigexp * Reg.region list * Reg.region list * Label.label * next (* first regions are for the context of a sigexp and second list is for the sigexp itself *)
      | LabSigExpDots   of part list

    and sigexp =
	SigExpBasic     of spec * Reg.region list * Label.label * next
      | SigExpId        of sigid * Label.label * next
      | SigExpRea       of labsigexp * ltreadesc * Reg.region list * Label.label * next
      | SigExpDots      of part list

    and labstrexp =
	LabStrExp       of strexp * Reg.region list * Reg.region list * Label.label * next (* first regions are for the context of a strexp and second list is for the strexp itself *)
      | LabStrExpDots   of part list

    and strexp =
	StrExpBasic     of strdec * Reg.region list * Label.label * next
      | StrExpId        of longstrid * Label.label * next
      | StrExpOp        of labstrexp * labsigexp * Reg.region * Label.label * next
      | StrExpTr        of labstrexp * labsigexp * Reg.region * Label.label * next
      | StrExpFExp      of funid  * labstrexp * Reg.region list * Label.label * next
      | StrExpFDec      of funid  * strdec * Reg.region list * Label.label * next
      | StrExpLocal     of strdec * labstrexp * Reg.region list * Label.label * next
      | StrExpDots      of part list

    and longtyconeq =
	LongTyConEq     of longtycon list * Reg.region list * Label.label * next
      | LongTyConEqDots of part list

    and longstrideq =
	LongStrIdEq     of longstrid list * Reg.region list * next
      | LongStrIdEqDots of part list

    and specone =
	SpecValue       of valdesc   * Reg.region * Label.label * next
      | SpecType        of typdesc   * Reg.region * Label.label * next
      | SpecEqtype      of typdesc   * Reg.region * Label.label * next
      | SpecException   of excdesc   * Reg.region * Label.label * next
      | SpecTdr         of tdrdesc   * Reg.region * Label.label * next (* the derived forms for types *)
      | SpecDat         of datdesc   * Reg.region * Label.label * next
      | SpecStr         of strdesc   * Reg.region * Label.label * next
      | SpecInc         of labsigexp * Reg.region * Label.label * next
      | SpecIsi         of sigidseq  * Reg.region * Label.label * next (* Iid stands for include signature structure ids *)
      | SpecRep         of tycon * longtycon   * Reg.region list * Label.label * next
      | SpecSha         of spec  * longtyconeq * Reg.region list * Label.label * next
      | SpecSsi         of spec  * longstrideq * Reg.region list * Label.label * next (* Ssi stands for Shared structure ids *)
      | SpecOneDots     of part list

    and spec =
	Spec            of specone list * next
      | SpecDots        of part list

    and condescone =
	ConDescOneId    of ident * next
      | ConDescOneOf    of labid * labtype * Reg.region * Label.label * next
      | ConDescOneNoOf  of ident * next
      | ConDescOneDots  of part list

    and condesc =
	ConDesc         of condescone list * Reg.region list * next
      | ConDescDots     of part list

    and datdescone =
	DatDescOne      of datname * condesc * Reg.region * Label.label * next
      | DatDescOneDots  of part list

    and datdesc =
	DatDesc         of datdescone list * Reg.region list * next
      | DatDescDots     of part list

    and strdescone =
	StrDescOne      of strid * labsigexp * Reg.region * Label.label * next
      | StrDescOneDots  of part list

    and strdesc =
	StrDesc         of strdescone list * Reg.region list * next
      | StrDescDots     of part list

    and valdescone =
	ValDescOne      of labid * labtype * Reg.region * Label.label * next
      | ValDescOneDots  of part list

    and valdesc =
	ValDesc         of valdescone list * Reg.region list * next
      | ValDescDots     of part list

    and typdescone =
	TypDescOne      of datname * Label.label * next
      | TypDescOneDots  of part list

    and typdesc =
	TypDesc         of typdescone list * Reg.region list * next
      | TypDescDots     of part list

    and tdrdescone =
	TdrDescOne      of datname * labtype * Reg.region * Label.label * next
      | TdrDescOneDots  of part list

    and tdrdesc =
	TdrDesc         of tdrdescone list * Reg.region list * next
      | TdrDescDots     of part list

    and excdescone =
	ExcDescOne      of ident * Label.label * next
      | ExcDescOf       of labid * labtype * Reg.region * Label.label * next
      | ExcDescOneDots  of part list

    and excdesc =
	ExcDesc         of excdescone list * Reg.region list * next
      | ExcDescDots     of part list

    and part =
	(*Expression*)
        PartExp         of exp
      | PartSige        of sigexp
      | PartStre        of strexp
      (*Type*)
      | PartType        of types
      | PartSeq         of typeRow
      (*Pattern*)
      | PartPat         of pat
      (*Spec*)
      | PartSpec        of specone
      (*Identifier.  (2010-03-13)Aren't they in the exp already?*)
      | PartLgid        of longid
      | PartIdTy        of identty
      | PartTyCon       of longtycon
      | PartLgsid       of longstrid
      | PartSigid       of sigid
      (*Overloading class*)
      | PartClass       of class
      (*SmlTes dec*)
      | PartTes         of smltes
      (*(2010-03-13)We need another treatment for declarations.  See testcase 446.*)
      | PartDec         of dec
      | PartSigd        of sigdec
      | PartStrd        of strdecone
    (*| PartFund        of fundec*)

    (*In the scons, the ids are those of the corresponding overloading classes.*)
    and scon =
	SconInt         of string * Id.id * Reg.region * Label.label * next
      | SconWord        of string * Id.id * Reg.region * Label.label * next
      | SconReal        of string * Id.id * Reg.region * Label.label * next
      | SconString      of string * Id.id * Reg.region * Label.label * next
      | SconChar        of string * Id.id * Reg.region * Label.label * next
      | SconDots

    and pcon =
	PconBool        of string * Id.id * Reg.region * Label.label * next
      | PconNil         of string * Id.id * Reg.region * Label.label * next
      | PconRef         of string * Id.id * Reg.region * Label.label * next
      | PconDots

    and labid =
	LabId           of ident * Reg.region list * Label.label * next
      | LabIdDots       of part list

    and ident =
	Ident           of string * Id.id * Reg.region * Label.label * next
      | IdentPcon       of pcon
      (*| IdentIn         of string * Reg.region * Label.label * next (* we don't want to use the Dots because we want to be able to slice a dots as well *)*)
      | IdentDots

    and labclass =
	LabClass        of class * Reg.region list * Label.label * next
      | LabClassDots    of part list

    and class =
	Class           of string * Id.id * Reg.region * Label.label * next
      | ClassDots

    and tyclass =
	TyClassCl       of labclass * Reg.region * Label.label * next
      | TyClassTy       of types * Label.label * next
      | TyClassDots     of part list

    and labtyclass =
	LabTyClass      of tyclass * Reg.region list * Label.label * next
      | LabTyClassDots  of part list

    and tyclassseq = (* We don't use the first two for now. *)
	TyClassSeqOne   of tyclass * Reg.region list * Label.label * next (* The region list is for the unique type that compose the row.  It is a list because a type can span over multiple lines. *)
      | TyClassSeqEm    of Reg.region * Label.label * next
      | TyClassSeqSeq   of labtyclass list * Reg.region list * Label.label * next
      | TyClassSeqDots  of part list

    and strid =
	StrId           of string * Id.id * Reg.region * Label.label * next
      (*| StrIdIn         of string * Reg.region * Label.label * next*)
      | StrIdDots

    and sigid =
	SigId           of string * Id.id * Reg.region * Label.label * next
      (*| SigIdIn         of string * Reg.region * Label.label * next*)
      | SigIdDots

    and funid =
	FunId           of string * Id.id * Reg.region * Label.label * next
      (*| FunIdIn         of string * Reg.region * Label.label * next*)
      | FunIdDots

    and tycon =
	TyCon           of string * Id.id * Reg.region * Label.label * next
      | TyConDots

    and tylab =
	TyLab           of string * Reg.region * Label.label * next (* we don't need the int, do we? *)
      | TyLabDots

    and longid =
	LongIdQual      of strid * longid * Reg.region * Label.label * next
      | LongIdId        of ident
      | LongIdDots      of part list

(*    and lablongid =
	LabLongId       of longid * Reg.region list * Reg.region list * Label.label * next (* The regions are as for LabExp *)
      | LabLongIdDots   of part list*)

    and longstrid =
	LongStrIdQual   of strid * longstrid * Reg.region * Label.label * next
      | LongStrIdId     of strid
      | LongStrIdDots   of part list

    and longtycon =
	LongTyConQual   of strid * longtycon * Reg.region * Label.label * next
      | LongTyConId     of tycon
      | LongTyConDots   of part list

    and labtypevar =
	LabTypeVar        of typevar * Reg.region list * Label.label * next
      | LabTypeVarDots    of typevar list

    and typevar =
	TypeVar         of string * Id.id * Reg.region * Label.label * next (* the int is a typevar *)
      |	EqualityTypeVar of string * Id.id * Reg.region * Label.label * next (* the int is a typevar *)
      | TypeVarDots

    and typevarseq =
	TypeVarSeqOne     of typevar * Reg.region * Label.label * next
      | TypeVarSeqEm      of Reg.region * Label.label * next
      | TypeVarSeqSeq     of labtypevar list * Reg.region list * Label.label * next
      | TypeVarSeqDots    of typevar list

    (* NO REG.REGION? *)
    (* Do we need this labtype in a constructor binding or a unique type row? *)
    (* and that the same for all the lab stuff - maybe we need more and maybe less *)
    and labtype =
	LabType         of types * Reg.region list * Label.label * next
      | LabTypeDots     of part list

    and tyfield =
	TyField           of tylab * labtype * Reg.region * Label.label * next
      | TyFieldDots       of part list

    and types =
	TypeOneVar      of typevar
      | TypeArrow       of labtype * labtype * Reg.region * Label.label * next
      | TypeTuple       of labtype list * Reg.region list * Label.label * next
      | TypeRecord      of tyfield list * Reg.region list * Reg.region list * Label.label * next
      | TypeSlRec       of tyfield list * Reg.region list * Label.label * next
      | TypeTyCon       of typeRow * longtycon * Reg.region list * Label.label * next
      | TypeParen       of labtype * Reg.region * Reg.region * Label.label * next
      | TypeDots        of part list

    and typeRow =
	TypeRowOne      of types * Reg.region list * Label.label * next (* The region list is for the unique type that compose the row.  It is a list because a type can span over multiple lines. *)
      | TypeRowEm       of Reg.region * Label.label * next
      | TypeRowSeq      of labtype list * Reg.region list * Label.label * next
      | TypeRowDots     of part list

    and conbind =
	ConBind         of ident * next
      | ConBindOf       of labid * labtype * Reg.region * Label.label * next
      | ConBindNoOf     of ident * next
      (* WTF is this good for?
       * It is used in the slicing algorithm to know if we have to include the =
       * and datatype keywords of a datatype declaration in the slice.
       * We don't include them if the conbinds contain just type information.
       * But if the conbinds contain id information then we include = and datatype
       * to stress that they come from a datatype declaration.
       * Such a ConBindNoOf is always generated from a ConBindOf.
       * Ideally it should be all handled by the presence or not of the labels. *)
      | ConBindDots     of part list

    (* don't we want the last Label.label for this one? *)
    and conbindseq =
	ConBindSeq      of conbind list
      | ConBindSeqDots  of part list

    and valbindcore =
	ValBindCore     of labpat * labexp * Reg.region * Label.label * next
      | ValBindCoreDots of part list

    and valbindseq =
	ValBindSeq      of valbindcore list * Reg.region list * next
      | ValBindSeqDots  of part list

    (* the Reg.region is for "rec" *)
    and valbind =
	ValBindRec      of valbindseq * Reg.region * Label.label * next
      | ValBind         of valbindseq
      | ValBindDots     of part list

    and datname =
	DatName         of typevarseq * tycon * Reg.region list * next
      | DatNameDots

    and ldatname = (* stands for long dat name because of the longtycon *)
	LDatName        of typevarseq * longtycon * Reg.region list * next
      | LDatNameDots

    and datbind =
	DatBind         of datname * conbindseq * Reg.region * Label.label * next
      | DatBindDots     of part list

    and datbindseq =
	DatBindSeq      of datbind list * Reg.region list * next
      | DatBindSeqDots  of part list

    and labatpat =
	LabAtPat        of atpat * Reg.region * Label.label * next
      (* The region should be a region list because even if unlikely,
       * the argument of a function can span on multiple lines.
       * This is stupid because anyway all the region list should only be unique regions.
       * Spliting a region that spans over multiple lines should be part of the interfaces. *)
      | LabAtPatDots    of part list

    and fmatch =
	FMatchId        of ident * bool * Reg.region (* true if infix *)
      | FMatchApp       of fmatch * labatpat * Reg.region list * Reg.region * Label.label * next
      (* FMatchSlApp is like FMatchDots.
       * Why don't we use FMatchDots?
       * Is it because we don't want to have dots on the left of the name of the function? *)
      | FMatchSlApp     of fmatch * labatpat * next
      (* FMatchNoApp is like FMatchDots.  Same remark as above *)
      | FMatchNoApp     of fmatch * next
      | FMatchDots

    and labfmatch =
	LabFMatch       of fmatch * Reg.region list * Label.label * next
      | LabFMatchSl     of fmatch * next
      | LabFMatchDots

    and fmatchty =
	FMatchT         of labfmatch
      | FMatchTTy       of labfmatch * labtype * Reg.region * Label.label * next
      | FMatchTDots

    and fvalbindcore =
	FValBindCore    of fmatchty * labexp * Reg.region * Label.label * next
      (*| FVBCoreTy       of labfmatch * labtype * labexp * Reg.region * Reg.region * Label.label * Label.label * next*)
      | FVBCoreDots     of part list

    (*|	FSlValBindCore  of fmatch * labexp * Reg.region * Label.label * next*)

    and fvalbindone =
	FValBindOne     of fvalbindcore list * Reg.region list * Label.label * next
      | FVBOneDots      of part list

    and fvalbind =
	FValBind        of fvalbindone list * Reg.region list * next
      | FValBindDots    of part list

    and typbind =
	TypBind         of datname * labtype * Reg.region * Label.label * next
      | TypBindDots     of part list

    and typbindseq =
	TypBindSeq      of typbind list * Reg.region list * next
      | TypBindSeqDots  of part list

    and exbind =
	ExBind          of ident * Label.label * next
      | ExBindOf        of labid * labtype * Reg.region * Label.label * next
      | ExBindEq        of labid * longid  * Reg.region * Label.label * next
      | ExBindNo        of ident * next (* WTF is that? *)
      | ExBindDots      of part list

    and exbindseq =
	ExBindSeq       of exbind list * Reg.region list * next
      | ExBindSeqDots   of part list

    and longstrseq =
	LongStrSeq      of longstrid list * next
      | LongStrSeqDots  of part list

    and sigidseq =
	SigIdSeq        of sigid list * next
      | SigIdSeqDots    of part list

    and identseq =
	IdentSeq        of ident list * next
      | IdentSeqDots    of part list

    and dec =
	DecVal          of typevarseq * valbind  * Reg.region * next
      | DecFVal         of typevarseq * fvalbind * Reg.region * next
      | DecDatType      of datbindseq * Reg.region * next
      | DecDatWith      of datbindseq * typbindseq * Reg.region list * Label.label * next
      | DecDatRep       of tycon * longtycon * Reg.region list * Label.label * next
      | DecType         of typbindseq * Reg.region * next
      | DecEx           of exbindseq  * Reg.region * next
      | DecOpen         of longstrseq * Reg.region * Label.label * next
      | DecLocal        of decs * decs * Reg.region list * Label.label * next
      | DecAbsType      of datbindseq * decs * Reg.region list * Label.label * next
      | DecAbsWith      of datbindseq * typbindseq * decs * Reg.region list * Label.label * next
      | DecInfix        of int * identseq * Reg.region * Label.label * next (* default is 0 *)
      | DecInfixr       of int * identseq * Reg.region * Label.label * next
      | DecNonfix       of identseq * Reg.region * Label.label * next
      | DecOverload     of labid * labtype * labtypevar * tyclassseq * Reg.region list * Label.label * next
      | DecClass        of labclass * tyclassseq * Reg.region * Label.label * next
      | DecDots         of part list

    and decs =
	Decs            of dec list * next
      | DecsDots        of part list

    and labexp =
	LabExp          of exp * Reg.region list * Reg.region list * Label.label * next (* the first list of regions is for the context of the expression and the second one is for the expression itself - it is a list because an expression can be on multiple lines *)
      | LabExpDots      of part list

    and expfield =
	ExpField          of tylab * labexp * Reg.region * Reg.region list * Label.label * next (* the region list is for the space between the field name and the equal sign of the field - and similarly for patterns *)
      | ExpFieldDots      of part list

    and seqexp =
	SeqExp          of labexp list * labexp * Reg.region * Reg.region list * Label.label * next (* the first region is for the last semicolumn *)
      | SeqExpSl        of part list * labexp * Reg.region * Label.label * next (* for a seq after slicing *)
      | SeqExpDots      of part list

    and atexp =
	AtExpId         of longid
      | AtExpScon       of scon
      | AtExpTuple      of labexp list * Reg.region list * Label.label * next
      | AtExpRecord     of expfield list * Reg.region list * Reg.region list * Label.label * next (* the first regions are for the braces and the others are for the commas *)
      | AtExpSlRec      of expfield list * Reg.region list * Label.label * next  (* for a record after slicing - set: val {x, ...} = {x = 1, y = 1, y = 1}.  Why don't we have that for patterns? *)
      | AtExpLet        of decs * labexp * Reg.region list * Label.label * next
      | AtExpDLet       of decs * seqexp * Reg.region list * Label.label * next
      | AtExpParen      of labexp * Reg.region * Reg.region * Label.label * next
      | AtExpList       of labexp list * Reg.region list * Label.label * next
      | AtExpProj       of tylab * Reg.region * Reg.region * Label.label * next (* the first region is for the hash, the second one for the label *)
      | AtExpSeq        of seqexp * Reg.region list * Label.label * next
      | AtExpQuote      of quote list * Reg.region list * Label.label * next
      | AtExpDots       of part list

    and exp =
	ExpAtExp        of atexp
      | ExpFn           of match * Reg.region * Label.label * next
      | ExpApp          of exp * atexp * Reg.region list * Reg.region * Reg.region * Label.label * next
      | ExpCase         of labexp * match * Reg.region * Reg.region * Label.label * next
      | ExpConsList     of Id.id * labexp * labexp * Reg.region * Label.label * next
      | ExpOp           of string * Id.id * labexp * labexp * Reg.region * Label.label * next
      | ExpOr           of labexp * labexp * Reg.region * Label.label * next
      | ExpAnd          of labexp * labexp * Reg.region * Label.label * next
      | ExpTyped        of labexp * labtype * Reg.region * Label.label * next
      | ExpIte          of labexp * labexp * labexp * Reg.region list * Label.label * next
      | ExpWhile        of labexp * labexp * Reg.region * Reg.region * Label.label * next
      | ExpRaise        of labexp * Reg.region * Label.label * next
      | ExpHandle       of labexp * match * Reg.region * Label.label * next
      | ExpDots         of part list

    and quote =
	Quote           of string * Reg.region * Label.label * next
      | Antiquote       of exp * Reg.region list * Label.label * next
      | QuoteDots       of part list

    and match =
	Match           of mrule list * Reg.region list * next
      | MatchDots       of part list

    and mrule =
	Mrule           of labpat * labexp * Reg.region * Label.label * next
      | MruleDots       of part list

    and labpat =
	LabPat          of pat * Reg.region list * Reg.region list * Label.label * next (* the regions are as for labexp *)
      | LabPatDots      of part list

    and identty =
	IdentTyId       of ident
      | IdentTyTy       of labid * labtype * Reg.region * Label.label * next
      | IdentTyDots     of part list

    and labidty =
	LabIdTy         of identty * Reg.region list * Label.label * next
      | LabIdTyDots     of part list

    and patfield =
	PatField          of tylab * labpat * Reg.region * Reg.region list * Label.label * next
      | PatFieldId        of identty * next
      | PatFieldAs        of labidty * labpat * Reg.region * Label.label * next
      | PatFieldWild      of Reg.region * Label.label * next
      | PatFieldDots      of part list

    and atpat =
	AtPatWild       of Reg.region * next
      | AtPatId         of longid
      | AtPatScon       of scon
      | AtPatTuple      of labpat list * Reg.region list * Label.label * next (* the list of regions is for the parentheses and commas *)
      | AtPatRecord     of patfield list * Reg.region list * Reg.region list * Label.label * next (* Why do we separate the two sets of regions? *)
      | AtPatParen      of labpat * Reg.region * Reg.region * Label.label * next
      | AtPatList       of labpat list * Reg.region list * Label.label * next
      | AtPatOr         of labpat list * Reg.region list * Label.label * next (* NOT SML SYNTAX BUT SMLNJ *)
      | AtPatDots       of part list

    and pat =
	PatAtPat        of atpat
      | PatApp          of longid * atpat * Reg.region list * Reg.region * Label.label * next
      | PatConsList     of Id.id * labpat * labpat * Reg.region * Label.label * next
      | PatOp           of string * Id.id * labpat * labpat * Reg.region * Label.label * next
      | PatTyped        of labpat * labtype * Reg.region * Label.label * next
      | PatAs           of labidty * labpat * Reg.region * Label.label * next
      | PatDots         of part list


    (* This is what is returned by the parser *)
    type packs = progs * Label.label * Id.assoc


    (* true if we want to print labels (plus extra parentheses), false otherwise *)
    val decPrint           : bool ref
    val getDecPrint        : unit -> bool
    val setDecPrint        : bool -> unit
    val printAstProgs      : progs -> string
    val printAstLongId     : longid -> string
    val printAstDec        : dec -> string
    (*val printNonBasProgs   : progs -> string*)
    (*val getpos_progs       : progs        -> Label.labels -> (Label.label * Reg.region list) list*)
    (*val getpos_slprogs     : progs        -> (Label.label * Reg.region list) list*)

    (* Extracts the non basis parts of a program *)
    val getNonBasProgs     : progs -> progs

    (* These functions are only used in Slice.sml by the slicing algorithm *)
    val getTypeVarSeqNext    : typevarseq     -> next option
    val getLabTypeNext     : labtype      -> next option
    val getTypeNext        : types        -> next option
    val getTyFieldNext       : tyfield        -> next option
    val getExpFieldNext      : expfield       -> next option
    val getPatFieldNext      : patfield       -> next option
    val getDecNext         : dec          -> next option
    val getDecsNext        : decs         -> next option
    val getTopDecNext      : topdec       -> next option
    val getValBindCoreNext : valbindcore  -> next option
    val getConBindNext     : conbind      -> next option
    val getDatBindNext     : datbind      -> next option
    val getDatBindSeqNext  : datbindseq   -> next option
    val getTypBindSeqNext  : typbindseq   -> next option
    val getLabExpNext      : labexp       -> next option
    val getExpNext         : exp          -> next option
    val getQuoteNext       : quote        -> next option
    val getMruleNext       : mrule        -> next option
    val getLabPatNext      : labpat       -> next option
    val getPatNext         : pat          -> next option
    val getAtPatNext       : atpat        -> next option
    val getFVBCoreNext     : fvalbindcore -> next option
    val getFVBOneNext      : fvalbindone  -> next option
    val getLabIdNext       : labid        -> next option
    val getIdentNext       : ident        -> next option
    val getLabClassNext    : labclass     -> next option
    val getClassNext       : class        -> next option
    val getLabTyClassNext  : labtyclass   -> next option
    val getTyLabNext       : tylab        -> next option
    val getDatNameNext     : datname      -> next option
    val getLDatNameNext    : ldatname     -> next option
    val getLTReaDOneNext   : ltreadescone -> next option
    val getTyConNext       : tycon        -> next option
    val getPartNext        : part         -> next option
    val getFMatchNext      : fmatch       -> next option
    val getTypBindNext     : typbind      -> next option
    val getExBindNext      : exbind       -> next option
    val getLabFMatchNext   : labfmatch    -> next option
    val getFMatchTyNext    : fmatchty     -> next option
    val getIdentTyNext     : identty      -> next option
    val getLabIdTyNext     : labidty      -> next option
    val getLongIdNext      : longid       -> next option
    val getStrIdNext       : strid        -> next option
    val getSigIdNext       : sigid        -> next option
    val getFunIdNext       : funid        -> next option
    val getLongTyConNext   : longtycon    -> next option
    val getTypeVarNext     : typevar      -> next option
    val getLabTypeVarNext  : labtypevar     -> next option
    val getSigExpNext      : sigexp       -> next option
    val getLabSigExpNext   : labsigexp    -> next option
    val getStrDecNext      : strdec       -> next option
    val getStrDecOneNext   : strdecone    -> next option
    val getLabStrExpNext   : labstrexp    -> next option
    val getStrExpNext      : strexp       -> next option
    val getSpecNext        : spec         -> next option
    val getSpecOneNext     : specone      -> next option
    val getStrDescOneNext  : strdescone   -> next option
    val getDatDescOneNext  : datdescone   -> next option
    val getValDescOneNext  : valdescone   -> next option
    val getTypDescOneNext  : typdescone   -> next option
    val getExcDescOneNext  : excdescone   -> next option
    val getTdrDescOneNext  : tdrdescone   -> next option
    val getLongStrIdNext   : longstrid    -> next option
    val getFunBONext       : funbindone   -> next option
    val getSigBONext       : sigbindone   -> next option
    val getStrBONext       : strbindone   -> next option
    val getConDescOneNext  : condescone   -> next option
    val getTopDecOneNext   : topdecone    -> next option
    val getProgOneNext     : progone      -> next option
    val getProgNext        : prog         -> next option
    (* Same for list things *)
    val getPartListNext      : part      list -> next option
    val getProgOneListNext   : progone   list -> next option
    val getTopDecOneListNext : topdecone list -> next option
    (* First ones *)
    val getTopDecOneFirst : topdecone -> Label.label option
    val getProgOneFirst   : progone   -> Label.label option
    val getProgFirst      : prog      -> Label.label option

    val getlabTypeVarseq     : typevarseq     -> Label.labels

    (*val getlabIdent        : ident        -> Label.labels*)
    val getlabDatName      : datname      -> Label.labels
    val getlabLDatName     : ldatname     -> Label.labels

    (* the next one is used by f_pat for an "as" pattern             *)
    (* to get the labels associated to to id on the left of the "as" *)
    val getlabelsLabIdTy   : labidty      -> Label.labels

    (* these are used to deal with field patterns *)
    val getlabstLabIdTy    : labidty      -> (Label.label * Label.label * string) option
    val getlabstIdentTy    : identty      -> (Label.label * Label.label * string) option
    val getlabstIdent      : ident        -> (Label.label * Label.label * string) option

    (* These are used by the f_longid function in Analyze.sml.
     * This is also now used by f_condesc and f_conbind to extract
     * the information on the constructors of datatypes. *)
    val getlabidStrId      : strid        -> Id.labelledId option
    val getlabidIdent      : ident        -> Id.labelledId option
    val getlabidLabId      : labid        -> Id.labelledId option
    val getlabidTyCon      : tycon        -> Id.labelledId option
    val getlabidLabclass   : labclass     -> Id.labelledId option

    (* Extract the top label of a labid *)
    val getLabelLabId      : labid        -> Label.labels
    (* Extract the labid/ident labels of a labid *)
    val getLabelsIdLabId   : labid        -> (Label.label * Label.label) option

    (* Check whether a longid is long *)
    val isLongIdent        : longid       -> bool

    (* Convert an (long) identifier into a Id.lid if it is not a dot term. *)
    val longidToLid        : longid       -> Id.lid option
    val longtyconToLid     : longtycon    -> Id.lid option
    val longstridToLid     : longstrid    -> Id.lid option
    val sigidToLid         : sigid        -> Id.lid option
    (*val stridToLid         : strid        -> Id.lid option*)

    (* True if a longid is a pcon *)
    val longidIsPcon       : longid       -> bool

    (* Extract the ident from a longid *)
    val longidToPcon       : longid       -> pcon option

    (* True is a longid is short *)
    val shortLongId        : longid       -> bool

    (* Extract the labels of a fvalbind.
     * A Label.label list is the list of labels of the arguments of
     * one branche of the fvalbind, from left to right. *)
    val getLabsFValLab     : fvalbindone -> Label.label list list

    (* The gettyvar* functions return a list with one member for each
     * occurrence of a type variable in the input expression.  Each
     * member of the list will is the variable (represented as the AST
     * subtree containing the occurrence) representing an exception
     * definition the type variable occurs within. *)
    val getTypeVarConbindseq : conbindseq   -> typevar list
    val getTypeVarType       : types        -> typevar list
    val getTypeVarLabType    : labtype      -> typevar list
    val getTypeVarDatName    : datname      -> typevar list
    val getTypeVarLDatName   : ldatname     -> typevar list
    val getTypeVarValBind    : valbind      -> typevar list
    val getTypeVarFValBind   : fvalbind     -> typevar list
    val getTypeVarExp        : exp          -> typevar list
    val getTypeVarLabExp     : labexp       -> typevar list
    val getTypeVarSigExp     : sigexp       -> typevar list
    val getTypeVarProg       : prog         -> typevar list
    val getTypeVarProgs      : progs        -> typevar list
    val getTypeVarValDesc    : valdesc      -> typevar list
    val getTypeVarConDesc    : condesc      -> typevar list
    val getTypeVarTypeVarSeq : typevarseq     -> typevar list

    (* returns the labels of the expressions which are not functions *)
    val isExpFnValBindSeq  : valbindseq   -> Label.labels list
    val isPatScon          : scon         -> Label.label option

    val extractFilesProg   : prog -> (string * Reg.region * bool) list
    val combineProg        : prog -> prog -> prog
    val isClearBasisProg   : prog -> bool

    val getNamesIdentSeq   : identseq -> string list

    val accessorListToJson : (ident * ident) list -> JSON.value
    val vizTraverse : progs -> Label.labels -> (ident * ident) list
end
