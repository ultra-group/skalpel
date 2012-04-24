(* Copyright 2009 2010 2011 2012 Heriot-Watt University
 *
 * Skalpe is a free software: you can redistribute it and/or modify
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
 *  o File name:   AstSML.sml
 *  o Description: This file contains the definition of our abstract
 *      syntax.  The file defines the structure AstSML which has
 *      signature ASTSML (defined in AstSML.sig).
 *)


structure AstSML :> ASTSML = struct

(* shorten the names of some structures *)
structure L  = Label
structure I  = Id
structure R  = Reg
structure EH = ErrorHandler
structure SY = SymbSlice

(* declare some new types *)
type next = L.label
type file = string
type bas  = bool

(* below are the various language definitons of Standard ML, most can be found
 * by looking at the definition of StandardML. Some of the forms here
 * are not present in that document because they are compiler specific
 * (an example of this would be the quasiquote system which is available
 * in the SML/NJ compiler for Standard ML. *)

datatype progs =
	 Progs      of (prog * file * bas * next) list

and prog =
    Prog            of progone list
  | ProgDots        of part list

and afile =
    AFile           of file * R.region * L.label * next
  | AFileDots

and abool =
    ABool           of string * R.region * L.label * next
  | ABoolDots

and progone =
    ProgOneDec      of topdec
  | ProgOneExp      of exp * I.id * R.region * L.label * next
  | ProgOneParse    of string * R.region list * L.label * next
  | ProgOneFile     of afile * next
  | ProgOneDots     of part list

and topdec =
    TopDec          of topdecone list
  | TopDecDots      of part list

and topdecone =
    TopDecOneTes    of smltes  * next
  | TopDecOneDec    of atopdec * next
  | TopDecOneDots   of part list

and smltes =
    SmlTesDec       of atopdec * R.region list * next
  | SmlTesSpec      of spec    * R.region list * next
  | SmlTesUse       of afile   * R.region list * next
  | SmlTesSBas      of afile   * R.region list * next
  | SmlTesCBas      of R.region list * L.label * next
  | SmlTesQuote     of abool   * R.region list * next
  | SmlTesType      of string  * R.region list * next
  | SmlTesDots      of part list

and atopdec =
    ATopDecStr      of strdec
  | ATopDecSig      of sigdec
  | ATopDecDots     of part list

(*and fundec =
    FunDec          of funbind * R.region * L.label * next
  | FunDecDots      of part list*)

and funbind =
    FunBind         of funbindone list * R.region list * next
  | FunBindDots     of part list

and funbindone =
    FunBindO        of funid * strid * labsigexp * labstrexp * R.region list * L.label * next
  | FunBindOO       of funid * strid * labsigexp * labsigexp * labstrexp * R.region list * L.label * next
  | FunBindOT       of funid * strid * labsigexp * labsigexp * labstrexp * R.region list * L.label * next
  | FunBindOS       of funid * spec  * labstrexp * R.region list * L.label * next
  | FunBindOSO      of funid * spec  * labsigexp * labstrexp * R.region list * L.label * next
  | FunBindOST      of funid * spec  * labsigexp * labstrexp * R.region list * L.label * next
  | FunBindODots    of part list

and sigdec =
    SigDec          of sigbind * R.region * next
  | SigDecDots      of part list

and sigbindone =
    SigBindOne      of sigid * labsigexp * R.region * L.label * next
  | SigBindOneDots  of part list

and sigbind =
    SigBind         of sigbindone list * R.region list * next
  | SigBindDots     of part list

and strdec =
    StrDec          of strdecone list * L.label * next
  | StrDecDots      of part list

and strdecone =
    StrDecOneDec    of decs
  | StrDecOneStr    of strbind * R.region * next
  | StrDecOneLoc    of strdec * strdec * R.region list * L.label * next
  | StrDecOneFun    of funbind * R.region * L.label * next
  | StrDecOneDots   of part list

and strbind =
    StrBind         of strbindone list * R.region list * next
  | StrBindDots     of part list

and strbindone =
    StrBindOneOp    of strid * labsigexp * labstrexp * R.region list * L.label * next
  | StrBindOneTr    of strid * labsigexp * labstrexp * R.region list * L.label * next
  | StrBindOne      of strid * labstrexp * R.region * L.label * next
  | StrBindOneDots  of part list

and ltreadescone =
    LTReaDOne       of ldatname * labtype * R.region list * L.label * next
  | LTReaDOneDots   of part list

and ltreadesc =
    LTReaDesc       of ltreadescone list * R.region list * L.label * next
  | LTReaDescDots   of part list

(*and treadescone =
    TReaDOne        of datname * labtype * R.region * L.label * next
  | TReaDOneDots    of part list

and treadesc =
    TReaDesc        of treadescone list * R.region list * next
  | TReaDescDots    of part list*)

and labsigexp =
    LabSigExp       of sigexp * R.region list * R.region list * L.label * next
  | LabSigExpDots   of part list

and sigexp =
    SigExpBasic     of spec * R.region list * L.label * next
  | SigExpId        of sigid * L.label * next
  | SigExpRea       of labsigexp * ltreadesc * R.region list * L.label * next
  | SigExpDots      of part list

and labstrexp =
    LabStrExp       of strexp * R.region list * R.region list * L.label * next
  | LabStrExpDots   of part list

and strexp =
    StrExpBasic     of strdec * R.region list * L.label * next
  | StrExpId        of longstrid * L.label * next
  | StrExpOp        of labstrexp * labsigexp * R.region * L.label * next
  | StrExpTr        of labstrexp * labsigexp * R.region * L.label * next
  | StrExpFExp      of funid  * labstrexp * R.region list * L.label * next
  | StrExpFDec      of funid  * strdec * R.region list * L.label * next
  | StrExpLocal     of strdec * labstrexp * R.region list * L.label * next
  | StrExpDots      of part list

and longtyconeq =
    LongTyConEq     of longtycon list * R.region list * L.label * next
  | LongTyConEqDots of part list

and longstrideq =
    LongStrIdEq     of longstrid list * R.region list * next
  | LongStrIdEqDots of part list

and sigidseq =
    SigIdSeq        of sigid list * next
  | SigIdSeqDots    of part list

and specone =
    SpecValue       of valdesc   * R.region * L.label * next
  | SpecType        of typdesc   * R.region * L.label * next
  | SpecEqtype      of typdesc   * R.region * L.label * next
  | SpecException   of excdesc   * R.region * L.label * next
  | SpecTdr         of tdrdesc   * R.region * L.label * next
  | SpecDat         of datdesc   * R.region * L.label * next
  | SpecStr         of strdesc   * R.region * L.label * next
  | SpecInc         of labsigexp * R.region * L.label * next
  | SpecIsi         of sigidseq  * R.region * L.label * next
  | SpecRep         of tycon * longtycon   * R.region list * L.label * next
  | SpecSha         of spec  * longtyconeq * R.region list * L.label * next
  | SpecSsi         of spec  * longstrideq * R.region list * L.label * next
  | SpecOneDots     of part list

and spec =
    Spec            of specone list * next
  | SpecDots        of part list

and condescone =
    ConDescOneId    of ident * next
  | ConDescOneOf    of labid * labtype * R.region * L.label * next
  | ConDescOneNoOf  of ident * next
  | ConDescOneDots  of part list

and condesc =
    ConDesc         of condescone list * R.region list * next
  | ConDescDots     of part list

and datdescone =
    DatDescOne      of datname * condesc * R.region * L.label * next
  | DatDescOneDots  of part list

and datdesc =
    DatDesc         of datdescone list * R.region list * next
  | DatDescDots     of part list

and strdescone =
    StrDescOne      of strid * labsigexp * R.region * L.label * next
  | StrDescOneDots  of part list

and strdesc =
    StrDesc         of strdescone list * R.region list * next
  | StrDescDots     of part list

and valdescone =
    ValDescOne      of labid * labtype * R.region * L.label * next
  | ValDescOneDots  of part list

and valdesc =
    ValDesc         of valdescone list * R.region list * next
  | ValDescDots     of part list

and typdescone =
    TypDescOne      of datname * L.label * next
  | TypDescOneDots  of part list

and typdesc =
    TypDesc         of typdescone list * R.region list * next
  | TypDescDots     of part list

and tdrdescone =
    TdrDescOne      of datname * labtype * R.region * L.label * next
  | TdrDescOneDots  of part list

and tdrdesc =
    TdrDesc         of tdrdescone list * R.region list * next
  | TdrDescDots     of part list

and excdescone =
    ExcDescOne      of ident * L.label * next
  | ExcDescOf       of labid * labtype * R.region * L.label * next
  | ExcDescOneDots  of part list

and excdesc =
    ExcDesc         of excdescone list * R.region list * next
  | ExcDescDots     of part list

and part =
    PartExp         of exp
  | PartDec         of dec
  | PartType        of types
  | PartSeq         of typeRow
  | PartPat         of pat
  | PartIdTy        of identty
  | PartTyCon       of longtycon
  | PartSpec        of specone
  | PartSige        of sigexp
  | PartStre        of strexp
  (*| PartFund        of fundec*)
  | PartSigd        of sigdec
  | PartStrd        of strdecone
  | PartLgid        of longid
  | PartLgsid       of longstrid
  | PartSigid       of sigid
  | PartTes         of smltes
  | PartClass       of class

and scon =
    SconInt         of string * I.id * R.region * L.label * next
  | SconWord        of string * I.id * R.region * L.label * next
  | SconReal        of string * I.id * R.region * L.label * next
  | SconString      of string * I.id * R.region * L.label * next
  | SconChar        of string * I.id * R.region * L.label * next
  | SconDots

and pcon =
    PconBool        of string * I.id * R.region * L.label * next
  | PconNil         of string * I.id * R.region * L.label * next
  | PconRef         of string * I.id * R.region * L.label * next
  | PconDots

and labid =
    LabId           of ident * R.region list * L.label * next
  | LabIdDots       of part list

and ident =
    Ident           of string * I.id * R.region * L.label * next
  | IdentPcon       of pcon
  | IdentDots

and labclass =
    LabClass        of class * R.region list * L.label * next
  | LabClassDots    of part list

and class =
    Class           of string * I.id * R.region * L.label * next
  | ClassDots

and tyclass =
    TyClassCl       of labclass * R.region * L.label * next
  | TyClassTy       of types * L.label * next
  | TyClassDots     of part list

and labtyclass =
    LabTyClass      of tyclass * R.region list * L.label * next
  | LabTyClassDots  of part list

and tyclassseq =
    TyClassSeqOne   of tyclass * R.region list * L.label * next
  | TyClassSeqEm    of R.region * L.label * next
  | TyClassSeqSeq   of labtyclass list * R.region list * L.label * next
  | TyClassSeqDots  of part list

and strid =
    StrId           of string * I.id * R.region * L.label * next
  | StrIdDots

and sigid =
    SigId           of string * I.id * R.region * L.label * next
  | SigIdDots

and funid =
    FunId           of string * I.id * R.region * L.label * next
  | FunIdDots

(* string is the string of the type constructor, eg
 * 1. 'Operators' 'datatype Operators = ADD | IF | LESS_THAN' *
 * 2. 'mytype' in 'type mytype = int'*)
and tycon =
    TyCon           of string * I.id * R.region * L.label * next
  | TyConDots

and tylab =
    TyLab           of string * R.region * L.label * next
  | TyLabDots

and longid =
    LongIdQual      of strid * longid * R.region * L.label * next
  | LongIdId        of ident
  | LongIdDots      of part list

and longstrid =
    LongStrIdQual   of strid * longstrid * R.region * L.label * next
  | LongStrIdId     of strid
  | LongStrIdDots   of part list

and longtycon =
    LongTyConQual   of strid * longtycon * R.region * L.label * next
  | LongTyConId     of tycon
  | LongTyConDots   of part list

and labtypevar =
    LabTypeVar        of typevar * R.region list * L.label * next
  | LabTypeVarDots    of typevar list

and typevar =
    TypeVar         of string * I.id * R.region * L.label * next
  | TypeVarDots

and typevarseq =
    TypeVarSeqOne     of typevar * R.region * L.label * next
  | TypeVarSeqEm      of R.region * L.label * next
  | TypeVarSeqSeq     of labtypevar list * R.region list * L.label * next
  | TypeVarSeqDots    of typevar list

and labtype =
    LabType         of types * R.region list * L.label * next
  | LabTypeDots     of part list

and tyfield =
    TyField           of tylab * labtype * R.region * L.label * next
  | TyFieldDots       of part list

and types =
    TypeOneVar      of typevar
  | TypeArrow       of labtype * labtype * R.region * L.label * next
  | TypeTuple       of labtype list * R.region list * L.label * next
  | TypeRecord      of tyfield list * R.region list * R.region list * L.label * next
  | TypeSlRec       of tyfield list * R.region list * L.label * next
  | TypeTyCon       of typeRow * longtycon * R.region list * L.label * next
  | TypeParen       of labtype * R.region * R.region * L.label * next
  | TypeDots        of part list

and typeRow =
    TypeRowOne      of types * R.region list * L.label * next
  | TypeRowEm       of R.region * L.label * next
  | TypeRowSeq      of labtype list * R.region list * L.label * next
  | TypeRowDots     of part list

and conbind =
    ConBind         of ident * next
  | ConBindOf       of labid * labtype * R.region * L.label * next
  | ConBindNoOf     of ident * next
  | ConBindDots     of part list

and conbindseq =
    ConBindSeq      of conbind list
  | ConBindSeqDots  of part list

and valbindcore =
    ValBindCore     of labpat * labexp * R.region * L.label * next
  | ValBindCoreDots of part list

and valbindseq =
    ValBindSeq      of valbindcore list * R.region list * next
  | ValBindSeqDots  of part list

and valbind =
    ValBindRec      of valbindseq * R.region * L.label * next
  | ValBind         of valbindseq
  | ValBindDots     of part list

and datname =
    DatName         of typevarseq * tycon * R.region list * next
  | DatNameDots

and ldatname =
    LDatName        of typevarseq * longtycon * R.region list * next
  | LDatNameDots

and datbind =
    DatBind         of datname * conbindseq * R.region * L.label * next
  | DatBindDots     of part list

and datbindseq =
    DatBindSeq      of datbind list * R.region list * next
  | DatBindSeqDots  of part list

and labatpat =
    LabAtPat        of atpat * R.region * L.label * next
  | LabAtPatDots    of part list

and fmatch =
    FMatchId        of ident * bool * R.region
  | FMatchApp       of fmatch * labatpat * R.region list * R.region * L.label * next
  | FMatchSlApp     of fmatch * labatpat * next
  | FMatchNoApp     of fmatch * next
  | FMatchDots

and labfmatch =
    LabFMatch       of fmatch * R.region list * L.label * next
  | LabFMatchSl     of fmatch * next
  | LabFMatchDots

and fmatchty =
    FMatchT         of labfmatch
  | FMatchTTy       of labfmatch * labtype * R.region * L.label * next
  | FMatchTDots

and fvalbindcore =
    FValBindCore    of fmatchty * labexp * R.region * L.label * next
  (*| FVBCoreTy       of labfmatch * labtype * labexp * R.region * R.region * L.label * next*)
  | FVBCoreDots     of part list

and fvalbindone =
    FValBindOne     of fvalbindcore list * R.region list * L.label * next
  | FVBOneDots      of part list

and fvalbind =
    FValBind        of fvalbindone list * R.region list * next
  | FValBindDots    of part list

and typbind =
    TypBind         of datname * labtype * R.region * L.label * next
  | TypBindDots     of part list

and typbindseq =
    TypBindSeq      of typbind list * R.region list * next
  | TypBindSeqDots  of part list

and exbind =
    ExBind          of ident * L.label * next
  | ExBindOf        of labid * labtype * R.region * L.label * next
  | ExBindEq        of labid * longid * R.region * L.label * next
  | ExBindNo        of ident * next
  | ExBindDots      of part list

and exbindseq =
    ExBindSeq       of exbind list * R.region list * next
  | ExBindSeqDots   of part list

and longstrseq =
    LongStrSeq      of longstrid list * next
  | LongStrSeqDots  of part list

and identseq =
    IdentSeq        of ident list * next
  | IdentSeqDots    of part list

and dec =
    DecVal          of typevarseq * valbind  * R.region * next
  | DecFVal         of typevarseq * fvalbind * R.region * next
  | DecDatType      of datbindseq * R.region * next
  | DecDatWith      of datbindseq * typbindseq * R.region list * L.label * next
  | DecDatRep       of tycon * longtycon * R.region list * L.label * next
  | DecType         of typbindseq * R.region * next
  | DecEx           of exbindseq  * R.region * next
  | DecOpen         of longstrseq * R.region * L.label * next
  | DecLocal        of decs * decs * R.region list * L.label * next
  | DecAbsType      of datbindseq * decs * R.region list * L.label * next
  | DecAbsWith      of datbindseq * typbindseq * decs * R.region list * L.label * next
  | DecInfix        of int * identseq * R.region * L.label * next
  | DecInfixr       of int * identseq * R.region * L.label * next
  | DecNonfix       of identseq * R.region * L.label * next
  | DecOverload     of labid * labtype * labtypevar * tyclassseq * R.region list * L.label * next
  | DecClass        of labclass * tyclassseq * R.region * L.label * next
  | DecDots         of part list

and decs =
    Decs            of dec list * next
  | DecsDots        of part list

and labexp =
    LabExp          of exp * R.region list * R.region list * L.label * next
  | LabExpDots      of part list

and expfield =
    ExpField          of tylab * labexp * R.region * R.region list * L.label * next
  | ExpFieldDots      of part list

(*and exprec =
    ExpRecSeq       of expfield list * R.region list * L.label * next
  | ExpRecSet       of expfield list
  | ExpRecDots      of part list*)

and seqexp =
    SeqExp          of labexp list * labexp * R.region * R.region list * L.label * next
  | SeqExpSl        of part list * labexp * R.region * L.label * next (* What is that good for? *)
  | SeqExpDots      of part list

and atexp =
    AtExpId         of longid
  | AtExpScon       of scon
  | AtExpTuple      of labexp list * R.region list * L.label * next
  | AtExpRecord     of expfield list * R.region list * R.region list * L.label * next
  | AtExpSlRec      of expfield list * R.region list * L.label * next
  | AtExpLet        of decs * labexp * R.region list * L.label * next
  | AtExpDLet       of decs * seqexp * R.region list * L.label * next
  | AtExpParen      of labexp * R.region * R.region * L.label * next
  | AtExpList       of labexp list * R.region list * L.label * next
  | AtExpProj       of tylab * R.region * R.region * L.label * next
  | AtExpSeq        of seqexp * R.region list * L.label * next
  | AtExpQuote      of quote list * R.region list * L.label * next
  | AtExpDots       of part list

and exp =
    ExpAtExp        of atexp
  | ExpFn           of match * R.region * L.label * next
  | ExpApp          of exp * atexp * R.region list * R.region * R.region * L.label * next
  | ExpCase         of labexp * match * R.region * R.region * L.label * next
  | ExpConsList     of I.id * labexp * labexp * R.region * L.label * next
  | ExpOp           of string * I.id * labexp * labexp * R.region * L.label * next
  | ExpOr           of labexp * labexp * R.region * L.label * next
  | ExpAnd          of labexp * labexp * R.region * L.label * next
  | ExpTyped        of labexp * labtype * R.region * L.label * next
  | ExpIte          of labexp * labexp * labexp * R.region list * L.label * next
  | ExpWhile        of labexp * labexp * R.region * R.region * L.label * next
  | ExpRaise        of labexp * R.region * L.label * next
  | ExpHandle       of labexp * match * R.region * L.label * next
  | ExpDots         of part list

and quote =
    Quote           of string * R.region * L.label * next
  | Antiquote       of exp * R.region list * L.label * next
  | QuoteDots       of part list

and match =
    Match           of mrule list * R.region list * next
  | MatchDots       of part list

and mrule =
    Mrule           of labpat * labexp * R.region * L.label * next
  | MruleDots       of part list

and labpat =
    LabPat          of pat * R.region list * R.region list * L.label * next
  | LabPatDots      of part list

and identty =
    IdentTyId       of ident
  | IdentTyTy       of labid * labtype * R.region * L.label * next
  | IdentTyDots     of part list

and labidty =
    LabIdTy         of identty * R.region list * L.label * next
  | LabIdTyDots     of part list

and patfield =
    PatField          of tylab * labpat * R.region * R.region list * L.label * next
  | PatFieldId        of identty * next
  | PatFieldAs        of labidty * labpat * R.region * L.label * next
  | PatFieldWild      of R.region * L.label * next
  | PatFieldDots      of part list

and atpat =
    AtPatWild       of R.region * next
  | AtPatId         of longid
  | AtPatScon       of scon
  | AtPatTuple      of labpat list * R.region list * L.label * next
  | AtPatRecord     of patfield list * R.region list * R.region list * L.label * next
  | AtPatParen      of labpat * R.region * R.region * L.label * next
  | AtPatList       of labpat list * R.region list * L.label * next
  | AtPatOr         of labpat list * R.region list * L.label * next
  | AtPatDots       of part list

and pat =
    PatAtPat        of atpat
  | PatApp          of longid * atpat * R.region list * R.region * L.label * next
  | PatConsList     of I.id * labpat * labpat * R.region * L.label * next
  | PatOp           of string * I.id * labpat * labpat * R.region * L.label * next
  | PatTyped        of labpat * labtype * R.region * L.label * next
  | PatAs           of labidty * labpat * R.region * L.label * next
  | PatDots         of part list


type packs = progs * L.label * I.assoc


(* printing section *)


val decPrint = ref true

fun getDecPrint _ = !decPrint
fun setDecPrint b = decPrint := b

val dots     = ".."
fun ldots  _ = if !decPrint then SY.ldots else ""
fun rdots  _ = if !decPrint then SY.rdots else ""
fun sep    _ = if !decPrint then " " else "\n"
fun rfdots l = if !decPrint
	       then "," ^ L.printLab l ^ SY.rdots
	       else ""
fun rpdots f = if !decPrint
	       then "," ^ f ^ SY.rdots
	       else ""


fun printAstProgs (Progs xs) = printAstProgList xs

and printAstProgList []                         = ""
  | printAstProgList [(prog, file, _, _)]       = ldots () ^ printAstProg prog ^ rpdots file
  | printAstProgList ((prog, file, _, _) :: xs) = ldots () ^ printAstProg prog ^ rpdots file ^ sep () ^ printAstProgList xs

and printAstProg (Prog pol)    = printAstProgOneList pol
  | printAstProg (ProgDots pl) = printAstPartList pl

and printAstProgOneList []        = ""
  | printAstProgOneList [p]       = printAstProgOne p
  | printAstProgOneList (p :: ps) = printAstProgOne p ^ ";" ^ printAstProgOneList ps

and printAstAFile (AFile (f, _, l, _)) = ldots () ^ f ^ rfdots l
  | printAstAFile AFileDots            = ldots () ^ dots ^ rdots ()

and printAstABool (ABool (b, _, l, _)) = ldots () ^ b ^ rfdots l
  | printAstABool ABoolDots            = ldots () ^ dots ^ rdots ()

and printAstProgOne (ProgOneDec td)              = printAstTopDec td
  | printAstProgOne (ProgOneExp (e, _, _, l, _)) = ldots () ^ printAstExp e ^ ";" ^ rfdots l
  | printAstProgOne (ProgOneParse (_, _, l, _))  = ldots () ^ "unparsable" ^ rfdots l
  | printAstProgOne (ProgOneFile (af, _))        = printAstAFile af
  | printAstProgOne (ProgOneDots pl)             = printAstPartList pl

and printAstTopDec (TopDec xs)     = printAstTopDecOneList xs
  | printAstTopDec (TopDecDots pl) = printAstPartList pl

and printAstTopDecOneList []        = ""
  | printAstTopDecOneList [x]       = printAstTopDecOne x
  | printAstTopDecOneList (x :: xs) = printAstTopDecOne x ^ " " ^ printAstTopDecOneList xs

and printAstTopDecOne (TopDecOneTes (x, _)) = printAstSmlTes x
  | printAstTopDecOne (TopDecOneDec (x, _)) = printAstATopDec x
  | printAstTopDecOne (TopDecOneDots pl)    = printAstPartList pl

and printAstSmlTes (SmlTesDec  (td, _, _)) = "SKALPEL-DEC"       ^ printAstATopDec td
  | printAstSmlTes (SmlTesSpec (sp, _, _)) = "SKALPEL-SPEC"      ^ printAstSpec    sp
  | printAstSmlTes (SmlTesUse  (af, _, _)) = "SKALPEL-USE-FILE"  ^ printAstAFile   af
  | printAstSmlTes (SmlTesSBas (af, _, _)) = "SKALPEL-SET-BASIS" ^ printAstAFile   af
  | printAstSmlTes (SmlTesCBas (_, l, _))  = ldots () ^ "SKALPEL-CLEAR-BASIS" ^ rfdots l
  | printAstSmlTes (SmlTesQuote (b, _,_))  = "SKALPEL-QUOTATION" ^ printAstABool b
  | printAstSmlTes (SmlTesType (st, _,_))  = "SKALPEL-TYPE     " ^ st
  | printAstSmlTes (SmlTesDots pl)         = printAstPartList pl

and printAstATopDec (ATopDecStr s)   = printAstStrDec s
  | printAstATopDec (ATopDecSig s)   = printAstSigDec s
  (*| printAstATopDec (TopDecOneFun f)   = printAstFunDec f*)
  | printAstATopDec (ATopDecDots pl) = printAstPartList pl

(*and printAstFunDec (FunDec (fb, _, l, _)) = ldots () ^ "functor " ^ printAstFunBind fb ^ rfdots l
  | printAstFunDec (FunDecDots pl)        = printAstPartList pl*)

and printAstFunBind (FunBind (fbol, _, _)) = printAstFunBindOneList fbol
  | printAstFunBind (FunBindDots pl)       = printAstPartList pl


and printAstFunBindOneList []        = ""
  | printAstFunBindOneList [x]       = printAstFunBindOne x
  | printAstFunBindOneList (x :: xs) = printAstFunBindOne x ^ " and " ^ printAstFunBindOneList xs

and printAstFunBindOne (FunBindO (fid, sid, si, se, _, l, _)) =
    ldots () ^ printAstFunId     fid ^
    " ("     ^ printAstStrId     sid ^
    " : "    ^ printAstLabSigExp si  ^
    ") = "   ^ printAstLabStrExp se  ^ rfdots l
  | printAstFunBindOne (FunBindOO (fid, sid, si, si', se, _, l, _)) =
    ldots () ^ printAstFunId     fid ^
    " ("     ^ printAstStrId     sid ^
    " : "    ^ printAstLabSigExp si  ^
    ") :> "  ^ printAstLabSigExp si' ^
    " = "    ^ printAstLabStrExp se  ^ rfdots l
  | printAstFunBindOne (FunBindOT (fid, sid, si, si', se, _, l, _)) =
    ldots () ^ printAstFunId     fid ^
    " ("     ^ printAstStrId     sid ^
    " : "    ^ printAstLabSigExp si  ^
    ") : "   ^ printAstLabSigExp si' ^
    " = "    ^ printAstLabStrExp se  ^ rfdots l
  | printAstFunBindOne (FunBindOS (fid, sp, se, _, l, _)) =
    ldots () ^ printAstFunId fid     ^
    " ("     ^ printAstSpec sp       ^
    ") = "   ^ printAstLabStrExp se  ^ rfdots l
  | printAstFunBindOne (FunBindOSO (fid, sp, si, se, _, l, _)) =
    ldots () ^ printAstFunId     fid ^
    " ("     ^ printAstSpec      sp  ^
    ") :> "  ^ printAstLabSigExp si  ^
    " = "    ^ printAstLabStrExp se  ^ rfdots l
  | printAstFunBindOne (FunBindOST (fid, sp, si, se, _, l, _)) =
    ldots () ^ printAstFunId     fid ^
    " ("     ^ printAstSpec      sp  ^
    ") : "   ^ printAstLabSigExp si  ^
    " = "    ^ printAstLabStrExp se  ^ rfdots l
  | printAstFunBindOne (FunBindODots pl) =
    printAstPartList pl

and printAstSigDec (SigDec (sb, _, _)) = "signature " ^ printAstSigBind sb
  | printAstSigDec (SigDecDots pl)     = printAstPartList pl

and printAstSigBind (SigBind (sbol, _, _)) = printAstSigBindOneList sbol
  | printAstSigBind (SigBindDots pl)       = printAstPartList pl

and printAstSigBindOneList []        = ""
  | printAstSigBindOneList [x]       = printAstSigBindOne x
  | printAstSigBindOneList (x :: xs) = printAstSigBindOne x ^ " and " ^ printAstSigBindOneList xs

and printAstSigBindOne (SigBindOne (id, se, _, l, _)) =
    ldots () ^ printAstSigId id ^ " = " ^ printAstLabSigExp se ^ rfdots l
  | printAstSigBindOne (SigBindOneDots pl)            =
    printAstPartList pl

and printAstStrDec (StrDec (xs, l, _)) = ldots () ^ printAstStrDecOneList xs ^ rfdots l
  | printAstStrDec (StrDecDots pl)     = printAstPartList pl

and printAstStrDecOneList []        = ""
  | printAstStrDecOneList [x]       = printAstStrDecOne x
  | printAstStrDecOneList (x :: xs) = printAstStrDecOne x ^ " " ^ printAstStrDecOneList xs

and printAstStrDecOne (StrDecOneDec d)                   =
    printAstDecs d
  | printAstStrDecOne (StrDecOneStr (sb, _, _))          =
    "structure " ^ printAstStrBind sb
  | printAstStrDecOne (StrDecOneLoc (sd1, sd2, _, l, _)) =
    ldots () ^ "local " ^ printAstStrDec sd1 ^ " in " ^ printAstStrDec sd2 ^ " end" ^ rfdots l
  | printAstStrDecOne (StrDecOneFun (fb, _, l, _))       =
    ldots () ^ "functor " ^ printAstFunBind fb ^ rfdots l
  | printAstStrDecOne (StrDecOneDots pl)                 =
    printAstPartList pl

and printAstStrBind (StrBind (xs, _, _)) = printAstStrBindOneList xs
  | printAstStrBind (StrBindDots pl)     = printAstPartList pl

and printAstStrBindOneList []        = ""
  | printAstStrBindOneList [x]       = printAstStrBindOne x
  | printAstStrBindOneList (x :: xs) = printAstStrBindOne x ^ " and " ^ printAstStrBindOneList xs

and printAstStrBindOne (StrBindOneOp (sid, si, st, _, l, _)) =
    ldots () ^ printAstStrId sid ^ " = " ^ printAstLabSigExp si ^ " :> " ^ printAstLabStrExp st ^ rfdots l
  | printAstStrBindOne (StrBindOneTr (sid, si, st, _, l, _)) =
    ldots () ^ printAstStrId sid ^ " = " ^ printAstLabSigExp si ^ " : " ^ printAstLabStrExp st ^ rfdots l
  | printAstStrBindOne (StrBindOne (sid, se, _, l, _))       =
    ldots () ^ printAstStrId sid ^ " = " ^ printAstLabStrExp se ^ rfdots l
  | printAstStrBindOne (StrBindOneDots pl)                   =
    printAstPartList pl

and printAstLTReaDOne (LTReaDOne (dn, ty, _, l, _)) =
    ldots () ^ "type " ^ printAstLDatName dn ^ " = " ^ printAstLabType ty ^ rfdots l
  | printAstLTReaDOne (LTReaDOneDots pl)            =
    printAstPartList pl

and printAstLTReaDOneList []        = ""
  | printAstLTReaDOneList [x]       = printAstLTReaDOne x
  | printAstLTReaDOneList (x :: xs) = printAstLTReaDOne x ^ " and " ^ printAstLTReaDOneList xs

and printAstLTReaDesc (LTReaDesc (xs, _, _, _)) = printAstLTReaDOneList xs
  | printAstLTReaDesc (LTReaDescDots pl)        = printAstPartList pl

(*and printAstTReaDOne (TReaDOne (dn, ty, _, l, _)) =
    ldots () ^ printAstDatName dn ^ " = " ^ printAstLabType ty ^ rfdots l
  | printAstTReaDOne (TReaDOneDots pl)            =
    printAstPartList pl

and printAstTReaDOneList []        = ""
  | printAstTReaDOneList [x]       = printAstTReaDOne x
  | printAstTReaDOneList (x :: xs) = printAstTReaDOne x ^ " and " ^ printAstTReaDOneList xs

and printAstTReaDesc (TReaDesc (xs, _, _)) = printAstTReaDOneList xs
  | printAstTReaDesc (TReaDescDots pl)     = printAstPartList pl*)

and printAstLabSigExp (LabSigExp (e, _, _, l, _)) = ldots () ^ printAstSigExp e ^ rfdots l
  | printAstLabSigExp (LabSigExpDots pl)          = ldots () ^ printAstPartList pl ^ rdots ()

and printAstSigExp (SigExpBasic (sp, _, l, _))    =
    ldots () ^ "sig " ^ printAstSpec sp ^ " end" ^ rfdots l
  | printAstSigExp (SigExpId (id, l, _))          =
    ldots () ^ printAstSigId id ^ rfdots l
  | printAstSigExp (SigExpRea (se, rea, _, l, _)) =
    ldots () ^ printAstLabSigExp se ^ " where " ^ printAstLTReaDesc rea ^ rfdots l
  | printAstSigExp (SigExpDots pl)                =
    printAstPartList pl

and printAstLabStrExp (LabStrExp (e, _, _, l, _)) = ldots () ^ printAstStrExp e ^ rfdots l
  | printAstLabStrExp (LabStrExpDots pl)          = ldots () ^ printAstPartList pl ^ rdots ()

and printAstStrExp (StrExpBasic (sd, _, l, _))     =
    ldots () ^ "struct " ^ printAstStrDec sd ^ " end" ^ rfdots l
  | printAstStrExp (StrExpId (id, l, _))           =
    ldots () ^ printAstLongStrId id ^ rfdots l
  | printAstStrExp (StrExpOp (se, si, _, l, _))    =
    ldots () ^ printAstLabStrExp se ^ " :> " ^ printAstLabSigExp si ^ rfdots l
  | printAstStrExp (StrExpTr (se, si, _, l, _))    =
    ldots () ^ printAstLabStrExp se ^ " : " ^ printAstLabSigExp si ^ rfdots l
  | printAstStrExp (StrExpFExp (id, se, _, l, _))  =
    ldots () ^ printAstFunId id ^ "(" ^ printAstLabStrExp se ^ ")" ^ rfdots l
  | printAstStrExp (StrExpFDec (id, sd, _, l, _))  =
    ldots () ^ printAstFunId id ^ "(" ^ printAstStrDec sd ^ ")" ^ rfdots l
  | printAstStrExp (StrExpLocal (sd, se, _, l, _)) =
    ldots () ^ "let " ^ printAstStrDec sd ^ " in " ^ printAstLabStrExp se ^ " end" ^ rfdots l
  | printAstStrExp (StrExpDots pl)                 =
    printAstPartList pl

and printAstLongTyConEqList []        = ""
  | printAstLongTyConEqList [x]       = printAstLongTyCon x
  | printAstLongTyConEqList (x :: xs) =
     printAstLongTyCon x ^ " = " ^ printAstLongTyConEqList xs

and printAstLongTyConEq (LongTyConEq (xs, _, _, _)) = printAstLongTyConEqList xs
  | printAstLongTyConEq (LongTyConEqDots pl)        = printAstPartList pl

and printAstLongStrIdEqList []        = ""
  | printAstLongStrIdEqList [x]       = printAstLongStrId x
  | printAstLongStrIdEqList (x :: xs) =
    printAstLongStrId x ^ " = " ^ printAstLongStrIdEqList xs

and printAstLongStrIdEq (LongStrIdEq (xs, _, _)) = printAstLongStrIdEqList xs
  | printAstLongStrIdEq (LongStrIdEqDots pl)     = printAstPartList pl

and printAstSigIdSeqList []        = ""
  | printAstSigIdSeqList [x]       = printAstSigId x
  | printAstSigIdSeqList (x :: xs) =
    printAstSigId x ^ " " ^ printAstSigIdSeqList xs

and printAstSigIdSeq (SigIdSeq (xs, _)) = printAstSigIdSeqList xs
  | printAstSigIdSeq (SigIdSeqDots pl)  = printAstPartList pl

and printAstSpecOne (SpecValue (vd, _, l, _))  =
    ldots () ^ "val " ^ printAstValDesc vd ^ rfdots l
  | printAstSpecOne (SpecType (td, _, l, _))  =
    ldots () ^ "type " ^ printAstTypDesc td ^ rfdots l
  | printAstSpecOne (SpecEqtype (td, _, l, _))  =
    ldots () ^ "eqtype " ^ printAstTypDesc td ^ rfdots l
  | printAstSpecOne (SpecException (ed, _, l, _))  =
    ldots () ^ "exception " ^ printAstExcDesc ed ^ rfdots l
  | printAstSpecOne (SpecTdr (td, _, l, _))  =
    ldots () ^ "type " ^ printAstTdrDesc td ^ rfdots l
  | printAstSpecOne (SpecDat (dd, _, l, _))  =
    ldots () ^ "datatype " ^ printAstDatDesc dd ^ rfdots l
  | printAstSpecOne (SpecStr (sd, _, l, _))  =
    ldots () ^ "structure " ^ printAstStrDesc sd ^ rfdots l
  | printAstSpecOne (SpecInc (si, _, l, _))  =
    ldots () ^ "include " ^ printAstLabSigExp si ^ rfdots l
  | printAstSpecOne (SpecIsi (si, _, l, _))  =
    ldots () ^ "include " ^ printAstSigIdSeq si ^ rfdots l
  | printAstSpecOne (SpecRep (tc, ltc, _, l, _)) =
    ldots () ^ "datatype " ^ printAstTyCon tc ^ " = datatype " ^ printAstLongTyCon ltc ^ rfdots l
  | printAstSpecOne (SpecSha (sp, tce, _, l, _)) =
    ldots () ^ printAstSpec sp ^ " sharing type " ^ printAstLongTyConEq tce ^ rfdots l
  | printAstSpecOne (SpecSsi (sp, tsi, _, l, _)) =
    ldots () ^ printAstSpec sp ^ " sharing " ^ printAstLongStrIdEq tsi ^ rfdots l
  | printAstSpecOne (SpecOneDots pl)         =
    printAstPartList pl

and printAstSpecList []        = ""
  | printAstSpecList [x]       = printAstSpecOne x
  | printAstSpecList (x :: xs) = (printAstSpecOne x) ^ ";" ^ (printAstSpecList xs)

and printAstSpec (Spec (spl, _)) = printAstSpecList spl
  | printAstSpec (SpecDots pl)   = printAstPartList pl

and printAstConDescOne (ConDescOneId (id, _))           =
    printAstIdent id
  | printAstConDescOne (ConDescOneOf (id, ty, _, l, _)) =
    ldots () ^ printAstLabId id ^ " of " ^ printAstLabType ty ^ rfdots l
  | printAstConDescOne (ConDescOneNoOf (id, _))         =
    printAstIdent id
  | printAstConDescOne (ConDescOneDots pl)              =
    printAstPartList pl

and printAstConDescOneList []        = ""
  | printAstConDescOneList [x]       = printAstConDescOne x
  | printAstConDescOneList (x :: xs) = printAstConDescOne x ^ " | " ^ printAstConDescOneList xs

and printAstConDesc (ConDesc (cdol, _, _)) = printAstConDescOneList cdol
  | printAstConDesc (ConDescDots pl)       = printAstPartList pl

and printAstDatDescOne (DatDescOne (dn, cd, _, l, _)) =
    ldots () ^ printAstDatName dn ^ " = " ^ printAstConDesc cd ^ rfdots l
  | printAstDatDescOne (DatDescOneDots pl) = printAstPartList pl

and printAstDatDescList []        = ""
  | printAstDatDescList [x]       = printAstDatDescOne x
  | printAstDatDescList (x :: xs) = printAstDatDescOne x ^ " and " ^ printAstDatDescList xs

and printAstDatDesc (DatDesc (ddl, _, _)) = printAstDatDescList ddl
  | printAstDatDesc (DatDescDots pl)      = printAstPartList pl

and printAstStrDescOne (StrDescOne (id, se, _, l, _)) =
    ldots () ^ printAstStrId id ^ " : " ^ printAstLabSigExp se ^ rfdots l
  | printAstStrDescOne (StrDescOneDots pl) = printAstPartList pl

and printAstStrDescList []        = ""
  | printAstStrDescList [x]       = printAstStrDescOne x
  | printAstStrDescList (x :: xs) = printAstStrDescOne x ^ " and " ^ printAstStrDescList xs

and printAstStrDesc (StrDesc (sdl, _, _)) = printAstStrDescList sdl
  | printAstStrDesc (StrDescDots pl)      = printAstPartList pl

and printAstValDescOne (ValDescOne (id, t, _, l, _)) =
    ldots () ^ printAstLabId id ^ " : " ^ printAstLabType t ^ rfdots l
  | printAstValDescOne (ValDescOneDots pl) = printAstPartList pl

and printAstValDescList []        = ""
  | printAstValDescList [x]       = printAstValDescOne x
  | printAstValDescList (x :: xs) = printAstValDescOne x ^ " and " ^ printAstValDescList xs

and printAstValDesc (ValDesc (vdl, _, _)) = printAstValDescList vdl
  | printAstValDesc (ValDescDots pl)      = printAstPartList pl

and printAstTypDescOne (TypDescOne (dn, l, _)) =
    ldots () ^ printAstDatName dn ^ rfdots l
  | printAstTypDescOne (TypDescOneDots pl) = printAstPartList pl

and printAstTypDescList []        = ""
  | printAstTypDescList [x]       = printAstTypDescOne x
  | printAstTypDescList (x :: xs) = printAstTypDescOne x ^ " and " ^ printAstTypDescList xs

and printAstTypDesc (TypDesc (tdl, _, _)) = printAstTypDescList tdl
  | printAstTypDesc (TypDescDots pl)      = printAstPartList pl

and printAstTdrDescOne (TdrDescOne (dn, ty, _, l, _)) =
    ldots () ^ printAstDatName dn ^ " = " ^ printAstLabType ty ^ rfdots l
  | printAstTdrDescOne (TdrDescOneDots pl) = printAstPartList pl

and printAstTdrDescList []        = ""
  | printAstTdrDescList [x]       = printAstTdrDescOne x
  | printAstTdrDescList (x :: xs) = printAstTdrDescOne x ^ " and " ^ printAstTdrDescList xs

and printAstTdrDesc (TdrDesc (tdl, _, _)) = printAstTdrDescList tdl
  | printAstTdrDesc (TdrDescDots pl)      = printAstPartList pl

and printAstExcDescOne (ExcDescOne (id, l, _)) =
    ldots () ^ printAstIdent id ^ rfdots l
  | printAstExcDescOne (ExcDescOf (id, t, _, l, _)) =
    ldots () ^ printAstLabId id ^ " of " ^ printAstLabType t ^ rfdots l
  | printAstExcDescOne (ExcDescOneDots pl) = printAstPartList pl

and printAstExcDescList []        = ""
  | printAstExcDescList [x]       = printAstExcDescOne x
  | printAstExcDescList (x :: xs) = printAstExcDescOne x ^ " and " ^ printAstExcDescList xs

and printAstExcDesc (ExcDesc (edl, _, _)) = printAstExcDescList edl
  | printAstExcDesc (ExcDescDots pl)      = printAstPartList pl

and printAstPart (PartExp   e) = printAstExp            e
  | printAstPart (PartDec   d) = printAstDec            d
  | printAstPart (PartType  t) = printAstType           t
  | printAstPart (PartSeq   s) = printAstTypeRow   s
  | printAstPart (PartPat   p) = printAstPat            p
  | printAstPart (PartIdTy  i) = printAstIdentTy        i
  | printAstPart (PartTyCon t) = printAstLongTyCon      t
  | printAstPart (PartSpec  s) = printAstSpecOne        s
  | printAstPart (PartSige  s) = printAstSigExp         s
  | printAstPart (PartStre  s) = printAstStrExp         s
  (*| printAstPart (PartFund  d) = printAstFunDec       d*)
  | printAstPart (PartSigd  s) = printAstSigDec         s
  | printAstPart (PartStrd  s) = printAstStrDecOne      s
  | printAstPart (PartLgid  i) = printAstLongId         i
  | printAstPart (PartLgsid i) = printAstLongStrId      i
  | printAstPart (PartSigid i) = printAstSigId          i
  | printAstPart (PartTes   t) = printAstSmlTes         t
  | printAstPart (PartClass c) = printAstClass          c

and printAstPartList []        = dots
  | printAstPartList (x :: xs) = dots ^ printAstPart x ^ printAstPartList xs

and printAstScon (SconInt    (st, _, _, lab, _)) = ldots () ^ st ^ rfdots lab
  | printAstScon (SconWord   (st, _, _, lab, _)) = ldots () ^ st ^ rfdots lab
  | printAstScon (SconReal   (st, _, _, lab, _)) = ldots () ^ st ^ rfdots lab
  | printAstScon (SconString (st, _, _, lab, _)) = ldots () ^ st ^ rfdots lab
  | printAstScon (SconChar   (st, _, _, lab, _)) = ldots () ^ st ^ rfdots lab
  | printAstScon SconDots                        = ldots () ^ dots ^ rdots ()

and printAstPcon (PconBool (st, _, _, l, _)) = ldots () ^ st ^ rfdots l
  | printAstPcon (PconNil  (st, _, _, l, _)) = ldots () ^ st ^ rfdots l
  | printAstPcon (PconRef  (st, _, _, l, _)) = ldots () ^ st ^ rfdots l
  | printAstPcon PconDots                    = ldots () ^ dots ^ rdots ()

and printAstLabId (LabId (id, _, lab, _)) = ldots () ^ printAstIdent id ^ rfdots lab
  | printAstLabId (LabIdDots pl)          = printAstPartList pl

and printAstIdent (Ident (st, _, _, l, _))   = ldots () ^ st ^ rfdots l
  | printAstIdent (IdentPcon pc)             = printAstPcon pc
  | printAstIdent IdentDots                  = ldots () ^ dots ^ rdots ()

and printAstLabClass (LabClass (cl, _, lab, _)) = ldots () ^ printAstClass cl ^ rfdots lab
  | printAstLabClass (LabClassDots pl)          = printAstPartList pl

and printAstClass (Class (st, _, _, l, _)) = ldots () ^ st ^ rfdots l
  | printAstClass ClassDots                = ldots () ^ dots ^ rdots ()

and printAstStrId (StrId (st, _, _, lab, _))   = ldots () ^ st ^ rfdots lab
  | printAstStrId StrIdDots                    = ldots () ^ dots ^ rdots ()

and printAstSigId (SigId (st, _, _, lab, _))   = ldots () ^ st ^ rfdots lab
  | printAstSigId SigIdDots                    = ldots () ^ dots ^ rdots ()

and printAstFunId (FunId (st, _, _, lab, _))   = ldots () ^ st ^ rfdots lab
  | printAstFunId FunIdDots                    = ldots () ^ dots ^ rdots ()

and printAstTyLab (TyLab (st, _, lab, _)) =
    ldots () ^ st ^ rfdots lab
  | printAstTyLab TyLabDots               = ldots () ^ dots ^ rdots ()

and printAstTyCon (TyCon (st, v, reg, lab, nxt)) =
    ldots () ^ st ^ rfdots lab
  | printAstTyCon TyConDots                      = ldots () ^ dots ^ rdots ()

and printAstLongId (LongIdQual (sid, lid, _, lab, _)) =
    ldots () ^ printAstStrId sid ^ "." ^ printAstLongId lid ^ rfdots lab
  | printAstLongId (LongIdId id) = printAstIdent id
  | printAstLongId (LongIdDots pl) = printAstPartList pl

and printAstLongStrId (LongStrIdQual (sid, lsid, _, lab, _)) =
    ldots () ^ printAstStrId sid ^ "." ^ printAstLongStrId lsid ^ rfdots lab
  | printAstLongStrId (LongStrIdId sid) = printAstStrId sid
  | printAstLongStrId (LongStrIdDots pl) = printAstPartList pl

and printAstLongTyCon (LongTyConQual (sid, ltc, _, lab, _)) =
    ldots () ^ printAstStrId sid ^ "." ^ printAstLongTyCon ltc ^ rfdots lab
  | printAstLongTyCon (LongTyConId tc) = printAstTyCon tc
  | printAstLongTyCon (LongTyConDots pl) = printAstPartList pl

and printAstTypeVar (TypeVar (st, n, reg, lab, nxt)) =
    ldots () ^ st ^ rfdots lab
  | printAstTypeVar TypeVarDots = ldots () ^ dots ^ rdots ()

and printAstTypeVarlist []          = ""
  | printAstTypeVarlist [ty]        = printAstTypeVar ty
  | printAstTypeVarlist (ty :: tyl) = printAstTypeVar ty ^ ", " ^ printAstTypeVarlist tyl

and printAstTypeVarlistdots []        = dots
  | printAstTypeVarlistdots (ty::tyl) = dots ^ printAstTypeVar ty ^ printAstTypeVarlistdots tyl

and printAstLabTypeVar (LabTypeVar (tv, _, lab, _)) =
    ldots () ^ printAstTypeVar tv ^ rfdots lab
  | printAstLabTypeVar (LabTypeVarDots tvl)         =
    ldots () ^ printAstTypeVarlistdots tvl ^ rdots ()

and printAstLabTypeVarList []        = ""
  | printAstLabTypeVarList [x]       = printAstLabTypeVar x
  | printAstLabTypeVarList (x :: xs) = printAstLabTypeVar x ^ ", " ^ printAstLabTypeVarList xs

and printAstTypeVarseq (TypeVarSeqOne (ty, _, lab, _)) =
    ldots () ^ printAstTypeVar ty ^ rfdots lab
  | printAstTypeVarseq (TypeVarSeqEm (_, lab, _))   =
    ldots () ^ rfdots lab
  | printAstTypeVarseq (TypeVarSeqSeq (ltvl, _, lab, _))  =
    ldots () ^ "(" ^ printAstLabTypeVarList ltvl ^ ")" ^ rfdots lab
  | printAstTypeVarseq (TypeVarSeqDots tvl)            =
    ldots ()  ^ printAstTypeVarlistdots tvl ^ rdots ()

and printAstTypelist []        = ""
  | printAstTypelist [ty]      = printAstType ty
  | printAstTypelist (ty::tyl) = printAstType ty ^ " * " ^ printAstTypelist tyl

and printAstLabType (LabType (t, _, l, _)) = ldots () ^ printAstType t ^ rfdots l
  | printAstLabType (LabTypeDots pl)       = printAstPartList pl

and printAstLabTypeList []        = ""
  | printAstLabTypeList [ty]      = printAstLabType ty
  | printAstLabTypeList (ty::tyl) = printAstLabType ty ^ " * " ^ printAstLabTypeList tyl

and printAstLabTypeSeq []        = ""
  | printAstLabTypeSeq [ty]      = printAstLabType ty
  | printAstLabTypeSeq (ty::tyl) = printAstLabType ty ^ ", " ^ printAstLabTypeSeq tyl

and printAstTyField (TyField (tl, lt, r, l, n)) =
    ldots () ^ printAstTyLab tl ^ ":" ^ printAstLabType lt ^ rfdots l
  | printAstTyField (TyFieldDots pl)            = printAstPartList pl

and printAstTyFieldList []        = ""
  | printAstTyFieldList [x]       = printAstTyField x
  | printAstTyFieldList (x :: xs) = printAstTyField x ^ "," ^ printAstTyFieldList xs

and printAstType (TypeOneVar tyv)                  = printAstTypeVar tyv
  | printAstType (TypeArrow (ty1, ty2, _, lab, _)) =
    ldots () ^ printAstLabType ty1 ^ " -> " ^ printAstLabType ty2 ^ rfdots lab
  | printAstType (TypeTuple (tyl, _, lab, _))      =
    ldots () ^ printAstLabTypeList tyl ^ rfdots lab
  | printAstType (TypeRecord (trl, _, _, lab, _))  =
    ldots () ^ "{" ^ printAstTyFieldList trl ^ "}" ^ rfdots lab
  | printAstType (TypeSlRec (trl, _, lab, _))      =
    ldots () ^ "{" ^ printAstTyFieldList trl ^ "}" ^ rfdots lab
  | printAstType (TypeTyCon (ts, ltc, _, lab, _))  =
    ldots () ^ printAstTypeRow ts ^ " " ^ printAstLongTyCon ltc ^ rfdots lab
  | printAstType (TypeParen (ty, _, _, lab, _))    =
    ldots () ^ "(" ^ printAstLabType ty ^ ")" ^ rfdots lab
  | printAstType (TypeDots pl)                     = ldots () ^ printAstPartList pl ^ rdots ()

and printAstTypeRow (TypeRowOne (ty, _, lab, _))  =
    ldots () ^ printAstType ty ^ rfdots lab
  | printAstTypeRow (TypeRowEm (_, lab, _))       =
    ldots () ^ rfdots lab
  | printAstTypeRow (TypeRowSeq (tyl, _, lab, _)) =
    ldots () ^ "(" ^ printAstLabTypeSeq tyl ^ ")" ^ rfdots lab
  | printAstTypeRow (TypeRowDots pl)              =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstTyClass (TyClassCl (cl, _, l, _)) =
    ldots () ^ printAstLabClass cl ^ rfdots l
  | printAstTyClass (TyClassTy (ty, l, _))    =
    ldots () ^ printAstType ty ^ rfdots l
  | printAstTyClass (TyClassDots pl)          =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstLabTyClass (LabTyClass (t, _, l, _)) = ldots () ^ printAstTyClass t ^ rfdots l
  | printAstLabTyClass (LabTyClassDots pl)       = printAstPartList pl

and printAstLabTyClassList []        = ""
  | printAstLabTyClassList [x]       = printAstLabTyClass x
  | printAstLabTyClassList (x :: xs) = printAstLabTyClass x ^ ", " ^ printAstLabTyClassList xs

and printAstTyClassSeq (TyClassSeqOne (tc, _, lab, _))  =
    ldots () ^ printAstTyClass tc ^ rfdots lab
  | printAstTyClassSeq (TyClassSeqEm (_, lab, _))       =
    ldots () ^ rfdots lab
  | printAstTyClassSeq (TyClassSeqSeq (tcl, _, lab, _)) =
    ldots () ^ "(" ^ printAstLabTyClassList tcl ^ ")" ^ rfdots lab
  | printAstTyClassSeq (TyClassSeqDots pl)              =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstConbind (ConBind (id, _))                   = printAstIdent id
  | printAstConbind (ConBindOf (id, ty, reg, lab, nxt)) =
    ldots () ^ printAstLabId id ^ " of " ^ printAstLabType ty ^ rfdots lab
  | printAstConbind (ConBindNoOf (id, _))               = printAstIdent id
  | printAstConbind (ConBindDots pl)                    =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstConbindseqlist  []         = ""
  | printAstConbindseqlist [tyc]       = printAstConbind tyc
  | printAstConbindseqlist (tyc::tycl) =
    printAstConbind tyc ^ " | " ^ printAstConbindseqlist tycl

and printAstConbindseq (ConBindSeq tycl)   =
    printAstConbindseqlist tycl
  | printAstConbindseq (ConBindSeqDots pl) =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstValBindCore (ValBindCore (p, e, _, l, _)) =
    ldots () ^ printAstLabPat p ^ " = " ^ printAstLabExp e ^ rfdots l
  | printAstValBindCore (ValBindCoreDots pl)          =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstValBindList []          = ""
  | printAstValBindList [vb]        = printAstValBindCore vb
  | printAstValBindList (vb :: vbl) = printAstValBindCore vb ^ " and " ^ printAstValBindList vbl

and printAstValBindSeq (ValBindSeq (vbs, _, _)) =
    printAstValBindList vbs
  | printAstValBindSeq (ValBindSeqDots pl)      =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstValbind (ValBindRec (vbs, _, l, _)) = ldots () ^ "rec " ^ printAstValBindSeq vbs ^ rfdots l
  | printAstValbind (ValBind vbs)               = printAstValBindSeq vbs
  | printAstValbind (ValBindDots pl)            = ldots () ^ printAstPartList pl ^ rdots ()

and printAstDatName (DatName (tvs, tn, _, _)) =
    printAstTypeVarseq tvs ^ " " ^ printAstTyCon tn
  | printAstDatName DatNameDots               = ldots () ^ dots ^ rdots ()

and printAstLDatName (LDatName (tvs, tn, _, _)) =
    printAstTypeVarseq tvs ^ " " ^ printAstLongTyCon tn
  | printAstLDatName LDatNameDots               = ldots () ^ dots ^ rdots ()

and printAstDatbind (DatBind (dtn, tycs, reg, lab, nxt)) =
    ldots () ^ printAstDatName dtn ^ " = " ^ printAstConbindseq tycs ^ rfdots lab
  | printAstDatbind (DatBindDots pl)                     =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstDatbindseqlist []        = ""
  | printAstDatbindseqlist [db]      = printAstDatbind db
  | printAstDatbindseqlist (db::dbl) =
    printAstDatbind db ^ " and " ^ printAstDatbindseqlist dbl

and printAstDatbindseq (DatBindSeq (dbl, regl, nxt)) = printAstDatbindseqlist dbl
  | printAstDatbindseq (DatBindSeqDots pl)           = ldots () ^ printAstPartList pl ^ rdots ()

and printAstOrPatList []        = ""
  | printAstOrPatList [x]       = printAstLabPat x
  | printAstOrPatList (x :: xs) = printAstLabPat x ^ " | " ^ printAstOrPatList xs

and printAstLabAtPat (LabAtPat (ap, _, l, _))   = ldots () ^ printAstAtpat ap ^ rfdots l
  | printAstLabAtPat (LabAtPatDots pl)          = ldots () ^ printAstPartList pl ^ rdots ()

and printAstFMatch (FMatchId (id, _, _))                = printAstIdent id
  | printAstFMatch (FMatchApp (fm, lap, _, _, l, _))    =
    ldots () ^ printAstFMatch fm ^ " " ^ printAstLabAtPat lap ^ rfdots l
  | printAstFMatch (FMatchSlApp (fm, lap, _))           = printAstFMatch fm ^ dots ^ printAstLabAtPat lap
  | printAstFMatch (FMatchNoApp (fm, _))                = printAstFMatch fm ^ dots
  | printAstFMatch FMatchDots                           = ldots () ^ dots ^ rdots ()

and printAstLabFMatch (LabFMatch (fm, _, l, _))          = ldots () ^ printAstFMatch fm ^ rfdots l
  (*| printAstLabFMatch (LabFMatchTy (fm, ty, _, _, l, _)) =
    ldots () ^ printAstFMatch fm ^ ":" ^ printAstLabType ty ^ "," ^ L.printelt l ^ "}"*)
  | printAstLabFMatch (LabFMatchSl (fm, _))              = printAstFMatch fm ^ dots
  | printAstLabFMatch LabFMatchDots                      = ldots () ^ dots ^ rdots ()

and printAstFMatchTy (FMatchT fm)                  = printAstLabFMatch fm
  | printAstFMatchTy (FMatchTTy (fm, ty, _, l, _)) =
    ldots () ^ printAstLabFMatch fm ^ ":" ^ printAstLabType ty ^ rfdots l
  | printAstFMatchTy FMatchTDots                   = ldots () ^ dots ^ rdots ()

and printAstFValbindcore (FValBindCore (fm, e, _, l, _))  =
    ldots () ^ printAstFMatchTy fm ^ " = " ^ printAstLabExp e ^ rfdots l
  (*| printAstFValbindcore (FVBCoreTy (lfm, t, e, _, _, l, _))  =
    printAstLabFMatch lfm ^ " : " ^ printAstLabType t ^ " {=," ^ L.printelt l ^ "} " ^ printAstLabExp e*)
  | printAstFValbindcore (FVBCoreDots pl)                 =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstFValbindcorelist []        = ""
  | printAstFValbindcorelist [x]       = printAstFValbindcore x
  | printAstFValbindcorelist (x :: xs) = printAstFValbindcore x ^ " | " ^ printAstFValbindcorelist xs

and printAstFValbindone (FValBindOne (fvbcl, _, l, _)) =
    ldots () ^ printAstFValbindcorelist fvbcl ^ rfdots l
  | printAstFValbindone (FVBOneDots pl)                =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstFValbindonelist []        = ""
  | printAstFValbindonelist [x]       = printAstFValbindone x
  | printAstFValbindonelist (x :: xs) = printAstFValbindone x ^ " and " ^ printAstFValbindonelist xs

and printAstFValbind (FValBind (fvbol, _, _)) = printAstFValbindonelist fvbol
  | printAstFValbind (FValBindDots pl)        = ldots () ^ printAstPartList pl ^ rdots ()

and printAstTypBind (TypBind (dn, ty, _, _, _)) = printAstDatName dn ^ " = " ^ printAstLabType ty
  | printAstTypBind (TypBindDots pl)            = ldots () ^ printAstPartList pl ^ rdots ()

and printAstTypBindList []        = ""
  | printAstTypBindList [x]       = printAstTypBind x
  | printAstTypBindList (x :: xs) = printAstTypBind x ^ " and " ^ printAstTypBindList xs

and printAstTypBindSeq (TypBindSeq (tbl, _, _)) = printAstTypBindList tbl
  | printAstTypBindSeq (TypBindSeqDots pl)      = ldots () ^ printAstPartList pl ^ rdots ()

and printAstExBind (ExBind (id, l, _))            = ldots () ^ printAstIdent id ^ rfdots l
  | printAstExBind (ExBindOf (id, t, _, l, _))    = ldots () ^ printAstLabId id ^ " of " ^ printAstLabType t ^ rfdots l
  | printAstExBind (ExBindEq (id, sid, _, l, _))  = ldots () ^ printAstLabId id ^ " = " ^ printAstLongId sid ^ rfdots l
  | printAstExBind (ExBindNo (id, _))             = ldots () ^ printAstIdent id ^ rdots ()
  | printAstExBind (ExBindDots pl)                = ldots () ^ printAstPartList pl ^ rdots ()

and printAstExBindList []        = ""
  | printAstExBindList [x]       = printAstExBind x
  | printAstExBindList (x :: xs) = printAstExBind x ^ " and " ^ printAstExBindList xs

and printAstExBindSeq (ExBindSeq (ebl, _, _)) = printAstExBindList ebl
  | printAstExBindSeq (ExBindSeqDots pl)      = ldots () ^ printAstPartList pl ^ rdots ()

and printAstLongStrIdList []        = ""
  | printAstLongStrIdList [x]       = printAstLongStrId x
  | printAstLongStrIdList (x :: xs) = printAstLongStrId x ^ " " ^ printAstLongStrIdList xs

and printAstLongStrSeq (LongStrSeq (labelledId, _)) = printAstLongStrIdList labelledId
  | printAstLongStrSeq (LongStrSeqDots pl)   = ldots () ^ printAstPartList pl ^ rdots ()

and printAstIdentList []        = ""
  | printAstIdentList [x]       = printAstIdent x
  | printAstIdentList (x :: xs) = printAstIdent x ^ " " ^ printAstIdentList xs

and printAstIdentSeq (IdentSeq (labelledId, _)) = printAstIdentList labelledId
  | printAstIdentSeq (IdentSeqDots pl)   = ldots () ^ printAstPartList pl ^ rdots ()

and printAstDec (DecVal (tvs, vb, _, _))                 = "val "       ^ printAstTypeVarseq tvs ^ " " ^ printAstValbind vb
  | printAstDec (DecFVal (tvs, fvb, _, _))               = "fun "       ^ printAstTypeVarseq tvs ^ " " ^ printAstFValbind fvb
  | printAstDec (DecDatType (dbs, _, _))                 = "datatype "  ^ printAstDatbindseq dbs
  | printAstDec (DecDatWith (db, tb, _, l, _))           = ldots () ^ "datatype " ^ printAstDatbindseq db ^ " withtype " ^ printAstTypBindSeq tb ^ rfdots l
  | printAstDec (DecDatRep (tc, ltc, _, l, _))           = ldots () ^ "datatype " ^ printAstTyCon tc ^ " = datatype " ^ printAstLongTyCon ltc ^ rfdots l
  | printAstDec (DecType (tbs, _, _))                    = "type "      ^ printAstTypBindSeq tbs
  | printAstDec (DecEx (ebs, _, _))                      = "exception " ^ printAstExBindSeq ebs
  | printAstDec (DecOpen (ids, _, l, _))                 = ldots () ^ "open "     ^ printAstLongStrSeq ids ^ rfdots l
  | printAstDec (DecLocal (d1, d2, _, l, _))             = ldots () ^ "local "    ^ printAstDecs d1 ^ " in " ^ printAstDecs d2 ^ " end" ^ rfdots l
  | printAstDec (DecAbsType (db, ds, _, l, _))           = ldots () ^ "abstype "  ^ printAstDatbindseq db ^ " with " ^ printAstDecs ds ^ " end" ^ rfdots l
  | printAstDec (DecAbsWith (b, t, d, _, l, _))          = ldots () ^ "abstype "  ^ printAstDatbindseq b ^ " withtype " ^ printAstTypBindSeq t ^ " with " ^ printAstDecs d ^ " end" ^ rfdots l
  | printAstDec (DecInfix  (i, ids, _, l, _))            = ldots () ^ "infix "    ^ Int.toString i ^ " " ^ printAstIdentSeq ids ^ rfdots l
  | printAstDec (DecInfixr (i, ids, _, l, _))            = ldots () ^ "infixr "   ^ Int.toString i ^ " " ^ printAstIdentSeq ids ^ rfdots l
  | printAstDec (DecNonfix (ids, _, l, _))               = ldots () ^ "nonfix "   ^ printAstIdentSeq ids ^ rfdots l
  | printAstDec (DecOverload (id, ty, tv, ts, _, l, _))  = ldots () ^ "overload "  ^ printAstLabId id ^ " : " ^ printAstLabType ty ^ " with " ^ printAstLabTypeVar tv ^ " in " ^ printAstTyClassSeq ts ^ rfdots l
  | printAstDec (DecClass (cl, ts, _, l, _))             = ldots () ^ "overload "  ^ printAstLabClass cl ^ " " ^ printAstTyClassSeq ts ^ rfdots l
  | printAstDec (DecDots pl)                             = ldots () ^ printAstPartList pl ^ rdots ()

and printAstDeclist []        = ""
  | printAstDeclist [d]       = printAstDec d
  | printAstDeclist (d :: ds) = printAstDec d ^ " " ^ printAstDeclist ds

and printAstDecs (Decs (ds, _)) = printAstDeclist ds
  | printAstDecs (DecsDots pl)  = ldots () ^ printAstPartList pl ^ rdots ()

and printAstExplist []            = ""
  | printAstExplist [exp]         = printAstExp exp
  | printAstExplist (exp :: expl) = printAstExp exp ^ "," ^ printAstExplist expl

and printAstLabExp (LabExp (e, _, _, l, _)) = ldots () ^ printAstExp e ^ rfdots l
  | printAstLabExp (LabExpDots pl)          = ldots () ^ printAstPartList pl ^ rdots ()

and printAstLabExpList []        = ""
  | printAstLabExpList [e]       = printAstLabExp e
  | printAstLabExpList (e :: el) = printAstLabExp e ^ "," ^ printAstLabExpList el

and printAstLabExpSeq []        = ""
  | printAstLabExpSeq [x]       = printAstLabExp x
  | printAstLabExpSeq (x :: xs) = printAstLabExp x ^ ";" ^ printAstLabExpSeq xs

and printAstExpField (ExpField (tl, e, r, rl, l, n)) =
    ldots () ^ printAstTyLab tl ^ " = " ^ printAstLabExp e ^ rfdots l
  | printAstExpField (ExpFieldDots pl)           = printAstPartList pl

and printAstExpFieldList []        = ""
  | printAstExpFieldList [x]       = printAstExpField x
  | printAstExpFieldList (x :: xs) = printAstExpField x ^ "," ^ printAstExpFieldList xs

and printAstExpFieldSet []        = dots
  | printAstExpFieldSet (x :: xs) = dots ^ printAstExpField x ^ printAstExpFieldSet xs

(*and printAstExpRec (ExpRecSeq (erl, _, lab, _)) = ldots () ^ printAstExpFieldList erl ^ rfdots lab
  | printAstExpRec (ExpRecSet erl)              = printAstExpFieldSet erl
  | printAstExpRec (ExpRecDots pl)              = printAstPartList pl*)

and printAstSeqExp (SeqExp (el, e, _, _, l, _))  = ldots () ^ "(" ^ printAstLabExpSeq (el @ [e]) ^ ")" ^ rfdots l
  | printAstSeqExp (SeqExpSl (pl, e, _, l, _))   = ldots () ^ "(" ^ printAstPartList pl ^ ";" ^ printAstLabExp e ^ ")" ^ rfdots l
  | printAstSeqExp (SeqExpDots pl)               = ldots () ^ printAstPartList pl ^ rdots ()

and printAstAtexp (AtExpId id)                    = printAstLongId id
  | printAstAtexp (AtExpScon sc)                  = printAstScon sc
  | printAstAtexp (AtExpTuple (expl, _, lab, _))  = ldots () ^ "(" ^ printAstLabExpList expl ^ ")" ^ rfdots lab
  | printAstAtexp (AtExpRecord (erl, _, _, l, _)) = ldots () ^ "{" ^ printAstExpFieldList erl  ^ "}" ^ rfdots l
  | printAstAtexp (AtExpSlRec (erl, _, lab, _))   = ldots () ^ "{" ^ printAstExpFieldList erl  ^ "}" ^ rfdots lab
  | printAstAtexp (AtExpLet (d, e, _, lab, _))    = ldots () ^
						    "let " ^ printAstDecs d   ^
						    " in " ^ printAstLabExp e ^
						    " end" ^ rfdots lab
  | printAstAtexp (AtExpDLet (d, s, _, lab, _))   = ldots () ^
						    "let " ^ printAstDecs d   ^
						    " in " ^ printAstSeqExp s ^
						    " end" ^ rfdots lab
  | printAstAtexp (AtExpParen (e, _, _, lab, _))  = ldots () ^ "(" ^ printAstLabExp e ^ ")" ^ rfdots lab
  | printAstAtexp (AtExpList (expl, _, lab, _))   = ldots () ^ "[" ^ printAstLabExpList expl ^ "]" ^ rfdots lab
  | printAstAtexp (AtExpProj (tl, _, _, l, _))    = ldots () ^ "#," ^ printAstTyLab tl ^ rfdots l
  | printAstAtexp (AtExpSeq (seq, _, l, _))       = ldots () ^ printAstSeqExp seq ^ rfdots l
  | printAstAtexp (AtExpQuote (quotes, _, l, _))  = ldots () ^ printAstQuotes quotes ^ rfdots l
  | printAstAtexp (AtExpDots pl)                  = ldots () ^ printAstPartList pl ^ rdots ()

and printAstQuotes []                = ""
  | printAstQuotes [quote]           = printAstQuote quote
  | printAstQuotes (quote :: quotes) = printAstQuote quote ^ "," ^ printAstQuotes quotes

and printAstQuote (Quote (st, _, lab, _))      = ldots () ^ st ^ rfdots lab
  | printAstQuote (Antiquote (exp, _, lab, _)) = ldots () ^ printAstExp exp ^ rfdots lab
  | printAstQuote (QuoteDots parts)            = ldots () ^ printAstPartList parts ^ rdots ()

and printAstExp (ExpAtExp ate)                      = printAstAtexp ate
  | printAstExp (ExpFn (mt, _, lab, _))             = ldots () ^ "fn " ^ printAstMatch mt ^ rfdots lab
  | printAstExp (ExpApp (exp, at, _, _, _, lab, _)) = ldots () ^ printAstExp exp ^ " " ^ printAstAtexp at ^ rfdots lab
  | printAstExp (ExpCase (e, m, _, _, l, _))        = ldots () ^ "case " ^ printAstLabExp e ^ " of " ^ printAstMatch m ^ rfdots l
  | printAstExp (ExpConsList (_, e1, e2, _, l, _))  = ldots () ^ printAstLabExp e1 ^ " :: " ^ printAstLabExp e2 ^ rfdots l
  | printAstExp (ExpOp (st, _, e1, e2, _, lab, _))  = ldots () ^ printAstLabExp e1 ^ " " ^ st ^ " " ^ printAstLabExp e2 ^ rfdots lab
  | printAstExp (ExpOr  (e1, e2, _, lab, _))        = ldots () ^ printAstLabExp e1 ^ " orelse "  ^ printAstLabExp e2 ^ rfdots lab
  | printAstExp (ExpAnd (e1, e2, _, lab, _))        = ldots () ^ printAstLabExp e1 ^ " andalso " ^ printAstLabExp e2 ^ rfdots lab
  | printAstExp (ExpTyped (e, t, _, l, _))          = ldots () ^ printAstLabExp e ^ " : " ^ printAstLabType t ^ rfdots l
  | printAstExp (ExpIte (x1, x2, x3, _, l, _))      = ldots () ^
						      "if "    ^ printAstLabExp x1 ^
						      " then " ^ printAstLabExp x2 ^
						      " else " ^ printAstLabExp x3 ^
						      rfdots l
  | printAstExp (ExpWhile (e1, e2, _, _, l, _))    = ldots () ^
						     "while " ^ printAstLabExp e1 ^
						     " do "   ^ printAstLabExp e2 ^
						     rfdots l
  | printAstExp (ExpRaise (e, _, l, _))            = ldots () ^ "raise " ^ printAstLabExp e ^ rfdots l
  | printAstExp (ExpHandle (e, m, _, l, _))        = ldots () ^ printAstLabExp e ^ " handle " ^ printAstMatch m ^ rfdots l
  | printAstExp (ExpDots pl)                       = ldots () ^ printAstPartList pl ^ rdots ()

and printAstMrulelist []        = ""
  | printAstMrulelist [mr]      = printAstMrule mr
  | printAstMrulelist (mr::mrl) = printAstMrule mr ^ " | " ^ printAstMrulelist mrl

and printAstMatch (Match (mrl, regl, nxt)) = printAstMrulelist mrl
  | printAstMatch (MatchDots pl)           = ldots () ^ printAstPartList pl ^ rdots ()

and printAstMrule (Mrule (pat, exp, reg, lab, nxt)) =
    ldots () ^ printAstLabPat pat ^ " => " ^ printAstLabExp exp ^ rfdots lab
  | printAstMrule (MruleDots pl)                    =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstPatlist []          = ""
  | printAstPatlist [pat]       = printAstPat pat
  | printAstPatlist (pat::patl) = printAstPat pat ^ "," ^ printAstPatlist patl

and printAstLabPat (LabPat (p, _, _, l, _)) = ldots () ^ printAstPat p ^ rfdots l
  | printAstLabPat (LabPatDots pl)          = ldots () ^ printAstPartList pl ^ rdots ()

and printAstLabPatList []          = ""
  | printAstLabPatList [pat]       = printAstLabPat pat
  | printAstLabPatList (pat::patl) = printAstLabPat pat ^ "," ^ printAstLabPatList patl

and printAstLabIdTy (LabIdTy (id, _, l, _)) = ldots () ^ printAstIdentTy id ^ rfdots l
  | printAstLabIdTy (LabIdTyDots pl)        = printAstPartList pl

and printAstPatField (PatField (tl, p, _, _, l, _))   =
    ldots () ^ printAstTyLab tl ^ " = " ^ printAstLabPat p ^ rfdots l
  | printAstPatField (PatFieldId (id, _))          = printAstIdentTy id
  | printAstPatField (PatFieldAs (id, p, _, l, _)) =
    ldots () ^ printAstLabIdTy id ^ " as " ^ printAstLabPat p ^ rfdots l
  | printAstPatField (PatFieldWild (r, l, n))      = ldots () ^ "..." ^ rfdots l
  | printAstPatField (PatFieldDots pl)             = printAstPartList pl

and printAstPatFieldList []        = ""
  | printAstPatFieldList [x]       = printAstPatField x
  | printAstPatFieldList (x :: xs) = printAstPatField x ^ "," ^ printAstPatFieldList xs

and printAstAtpat (AtPatWild _)                    = "_"
  | printAstAtpat (AtPatId id)                     = printAstLongId id
  | printAstAtpat (AtPatScon sc)                   = printAstScon sc
  | printAstAtpat (AtPatTuple (patl, _, l, _))     = ldots () ^ "(" ^ printAstLabPatList patl ^ ")" ^ rfdots l
  | printAstAtpat (AtPatRecord (prl, _, _, l, _))  = ldots () ^ "{" ^ printAstPatFieldList prl ^ "}" ^ rfdots l
  | printAstAtpat (AtPatParen (pat, _, _, lab, _)) = ldots () ^ "(" ^ printAstLabPat pat ^ ")" ^ rfdots lab
  | printAstAtpat (AtPatList (patl, _, lab, _))    = ldots () ^ "[" ^ printAstLabPatList patl ^ "]" ^ rfdots lab
  | printAstAtpat (AtPatOr (xs, _, l, _))          = ldots () ^ "(" ^ printAstOrPatList xs ^ ")" ^ rfdots l
  | printAstAtpat (AtPatDots pl)                   = ldots () ^ printAstPartList pl ^ rdots ()

and printAstIdentTy (IdentTyId id)               = printAstIdent id
  | printAstIdentTy (IdentTyTy (id, t, _, l, _)) =
    ldots () ^ printAstLabId id ^ " : " ^ printAstLabType t ^ rfdots l
  | printAstIdentTy (IdentTyDots pl)             =
    ldots () ^ printAstPartList pl ^ rdots ()

and printAstPat (PatAtPat (atp))                         = printAstAtpat atp
  | printAstPat (PatApp (id, atp, _, _, lab, _))         =
    ldots () ^ printAstLongId id ^ " " ^ printAstAtpat atp ^ rfdots lab
  | printAstPat (PatConsList (_, p1, p2, reg, lab, nxt)) =
    ldots () ^ printAstLabPat p1 ^ " :: " ^ printAstLabPat p2 ^ rfdots lab
  | printAstPat (PatOp (st, _, p1, p2, reg, lab, nxt))   =
    ldots () ^ printAstLabPat p1 ^ " " ^ st ^ " " ^ printAstLabPat p2 ^ rfdots lab
  | printAstPat (PatTyped (p, t, _, l, _))               =
    ldots () ^ printAstLabPat p ^ " : " ^ printAstLabType t ^ rfdots l
  | printAstPat (PatAs (id, p, _, l, _))                 =
    ldots () ^ printAstLabIdTy id ^ " as " ^ printAstLabPat p ^ rfdots l
  | printAstPat (PatDots pl)                             =
    ldots () ^ printAstPartList pl ^ rdots ()


(* Similar to printAstProgs, but does not print the basis *)

fun printNonBasProgs (Progs xs) = printNonBasProgList xs
and printNonBasProgList [] = ""
  | printNonBasProgList [(prog, file, _, _)] =
    let val f = #file (OS.Path.splitDirFile file)
    in if f = "basis.sml"
       then ""
       else ldots () ^ printAstProg prog ^ rpdots file
    end
  | printNonBasProgList ((prog, file, _, _) :: xs) =
    let val f = #file (OS.Path.splitDirFile file)
    in if f = "basis.sml"
       then printNonBasProgList xs
       else ldots () ^ printAstProg prog ^ rpdots file ^ sep () ^ printNonBasProgList xs
    end


(* Extracts the non basis part of a program. *)

fun getNonBasProgs (Progs xs) = Progs (getNonBasProgList xs)
and getNonBasProgList [] = []
  | getNonBasProgList ((prog, file, x, y) :: xs) =
    let val f = #file (OS.Path.splitDirFile file)
    in if f = "basis.sml"
       then getNonBasProgList xs
       else (prog, file, x, y) :: (getNonBasProgList xs)
    end


(* Extracting the labels of fval bindings *)

and getLabsFValFMatch (FMatchId _) = SOME []
  | getLabsFValFMatch (FMatchApp (fmatch, _, _, _, lab, _)) =
    (SOME ((Option.valOf (getLabsFValFMatch fmatch)) @ [lab])
     handle Option => NONE)
  | getLabsFValFMatch (FMatchSlApp _) = NONE
  | getLabsFValFMatch (FMatchNoApp _) = NONE
  | getLabsFValFMatch FMatchDots = NONE

and getLabsFValLabFMatch (LabFMatch (fmatch, _, lab, _)) =
    (SOME ((Option.valOf (getLabsFValFMatch fmatch)) @ [lab])
     handle Option => NONE)
  | getLabsFValLabFMatch (LabFMatchSl _) = NONE
  | getLabsFValLabFMatch LabFMatchDots = NONE

and getLabsFValFMatchTy (FMatchT labfmatch) =
    getLabsFValLabFMatch labfmatch
  | getLabsFValFMatchTy (FMatchTTy (labfmatch, _, _, _, _)) =
    getLabsFValLabFMatch labfmatch
  | getLabsFValFMatchTy FMatchTDots = NONE

and getLabsFValLabCore (FValBindCore (fmatchty, _, _, _, _)) =
    getLabsFValFMatchTy fmatchty
  | getLabsFValLabCore (FVBCoreDots _) = NONE

and getLabsFValLab (FValBindOne (fvalbindcores, _, _, _)) =
    List.mapPartial getLabsFValLabCore fvalbindcores
  | getLabsFValLab (FVBOneDots _) = []


(* Check wether a longid is long or not. *)
fun isLongIdent (LongIdQual _) = true
  | isLongIdent _ = false


(* Transforms a long ids into a lids *)

fun pconToLid (PconBool (_, v, _, l, _)) = SOME (I.ID (v, l))
  | pconToLid (PconNil  (_, v, _, l, _)) = SOME (I.ID (v, l))
  | pconToLid (PconRef  (_, v, _, l, _)) = SOME (I.ID (v, l))
  | pconToLid PconDots                   = NONE

fun idToLid (Ident (s, v, _, l, _)) =
    if String.isPrefix "_" s
    then NONE
    else SOME (I.ID (v, l))
  | idToLid (IdentPcon pc) = pconToLid pc
  | idToLid IdendDots = NONE

fun tyconToLid (TyCon (s, v, _, l, _)) =
    if String.isPrefix "_" s
    then NONE
    else SOME (I.ID (v, l))
  | tyconToLid TyConDots = NONE

fun sigidToLid (SigId (s, v, _, l, _)) =
    if String.isPrefix "_" s
    then NONE
    else SOME (I.ID (v, l))
  | sigidToLid SigIdDots = NONE

fun stridToLid (StrId (s, v, _, l, _)) =
    if String.isPrefix "_" s
    then NONE
    else SOME (I.ID (v, l))
  | stridToLid StrIdDots = NONE

fun longidToLid (LongIdQual (sid, lid, _, lab, _)) =
    (case (stridToLid sid, longidToLid lid) of
	 (SOME (I.ID x), SOME y) => SOME (I.LID (x, y, lab))
       | _ => NONE)
  | longidToLid (LongIdId sid) = idToLid sid
  | longidToLid (LongIdDots _) = NONE

fun longtyconToLid (LongTyConQual (sid, lid, _, lab, _)) =
    (case (stridToLid sid, longtyconToLid lid) of
	 (SOME (I.ID x), SOME y) => SOME (I.LID (x, y, lab))
       | _ => NONE)
  | longtyconToLid (LongTyConId sid) = tyconToLid sid
  | longtyconToLid (LongTyConDots _) = NONE

fun longstridToLid (LongStrIdQual (sid, lid, _, lab, _)) =
    (case (stridToLid sid, longstridToLid lid) of
	 (SOME (I.ID x), SOME y) => SOME (I.LID (x, y, lab))
       | _ => NONE)
  | longstridToLid (LongStrIdId sid) = stridToLid sid
  | longstridToLid (LongStrIdDots _) = NONE


(* true if a long id is a pcon *)

fun longidIsPcon (LongIdId (IdentPcon _)) = true
  | longidIsPcon _ = false


(* true if a long id is not long *)

fun shortLongId (LongIdId _) = true
  | shortLongId _ = false


(* Extracts the id from a lid *)

fun pconToPcon PconDots = NONE
  | pconToPcon x = SOME x

fun identToPcon (IdentPcon pc) = pconToPcon pc
  | identToPcon _ = NONE

fun longidToPcon (LongIdQual (_, lid, _, _, _)) =
    longidToPcon lid
  | longidToPcon (LongIdId id) = identToPcon id
  | longidToPcon (LongIdDots _) = NONE



(* get labels from some terms *)

fun getlabIdent (Ident (_, _, _, l, _))   = L.singleton l
  | getlabIdent (IdentPcon pc)            = getlabPcon pc
  | getlabIdent IdentDots                 = L.empty

and getlabPcon (PconBool (_, _, _, l, _)) = L.singleton l
  | getlabPcon (PconNil  (_, _, _, l, _)) = L.singleton l
  | getlabPcon (PconRef  (_, _, _, l, _)) = L.singleton l
  | getlabPcon PconDots                   = L.empty

fun getlabTypeVarseq (TypeVarSeqOne (_, _, l, _)) = L.singleton l
  | getlabTypeVarseq (TypeVarSeqEm (_, l, _))     = L.singleton l
  | getlabTypeVarseq (TypeVarSeqSeq (_, _, l, _)) = L.singleton l
  | getlabTypeVarseq (TypeVarSeqDots _)           = L.empty

fun getlabDatName (DatName (tvs, _, _, _)) = getlabTypeVarseq tvs
  | getlabDatName DatNameDots              = L.empty

fun getlabLDatName (LDatName (tvs, _, _, _)) = getlabTypeVarseq tvs
  | getlabLDatName LDatNameDots              = L.empty


(* get labels of a labidty *)


(*fun getlabelsPcon (PconBool (_, _, _, l, _)) = L.singleton l
  | getlabelsPcon (PconNil  (_, _, _, l, _)) = L.singleton l
  | getlabelsPcon (PconRef  (_, _, _, l, _)) = L.singleton l
  | getlabelsPcon PconDots                   = L.empty

fun getlabelsIdent (Ident (_, _, _, l, _))   = L.singleton l
  | getlabelsIdent (IdentPcon pc)            = getlabelsPcon pc
  | getlabelsIdent IdentDots                 = L.empty*)

fun getlabelsIdent _ = L.empty

fun getlabelsLabId (LabId (id, _, l, _)) = L.cons l (getlabelsIdent id)
  | getlabelsLabId (LabIdDots _)         = L.empty

fun getlabelsIdentTy (IdentTyId id)               = getlabelsIdent id
  | getlabelsIdentTy (IdentTyTy (id, _, _, l, _)) = L.cons l (getlabelsLabId id)
  | getlabelsIdentTy (IdentTyDots _)              = L.empty

fun getlabelsLabIdTy (LabIdTy (id, _, l, _)) = L.cons l (getlabelsIdentTy id)
  | getlabelsLabIdTy (LabIdTyDots _)         = L.empty




(* get label and name of an identifier *)


fun getlabstPcon (PconBool (s, _, _, l, _)) = SOME (l, l, s)
  | getlabstPcon (PconNil  (s, _, _, l, _)) = SOME (l, l, s)
  | getlabstPcon (PconRef  (s, _, _, l, _)) = SOME (l, l, s)
  | getlabstPcon PconDots                   = NONE

fun getlabstIdent (Ident (s, _, _, l, _)) = SOME (l, l, s)
  | getlabstIdent (IdentPcon pc)          = getlabstPcon pc
  | getlabstIdent IdentDots               = NONE

fun getlabstLabId (LabId (id, _, l, _)) = (case getlabstIdent id of NONE => NONE
								  | SOME (_, l', s) => SOME (l, l', s))
  | getlabstLabId (LabIdDots _)         = NONE

fun getlabstIdentTy (IdentTyId id)               = getlabstIdent id
  | getlabstIdentTy (IdentTyTy (id, _, _, _, _)) = getlabstLabId id
  | getlabstIdentTy (IdentTyDots _)              = NONE

fun getlabstLabIdTy (LabIdTy (id, _, _, _)) = getlabstIdentTy id
  | getlabstLabIdTy (LabIdTyDots _)         = NONE


(* get id and lab of a strid *)


fun getlabidStrId (StrId (_, n, _, l, _)) = SOME (n, l)
  | getlabidStrId StrIdDots               = NONE

fun getlabidPcon (PconBool (_, n, _, l, _)) = SOME (n, l)
  | getlabidPcon (PconNil  (_, n, _, l, _)) = SOME (n, l)
  | getlabidPcon (PconRef  (_, n, _, l, _)) = SOME (n, l)
  | getlabidPcon PconDots                   = NONE

fun getlabidIdent (Ident (_, n, _, l, _)) = SOME (n, l)
  | getlabidIdent (IdentPcon pc)          = getlabidPcon pc
  | getlabidIdent IdentDots               = NONE

fun getlabidLabId (LabId (id, _, _, _)) = getlabidIdent id
  | getlabidLabId (LabIdDots _)         = NONE

fun getlabidTyCon (TyCon (_, n, _, l, _)) = SOME (n, l)
  | getlabidTyCon TyConDots               = NONE

fun getlabidClass (Class (_, id, _, lab, _)) = SOME (id, lab)
  | getlabidClass ClassDots                  = NONE

fun getlabidLabclass (LabClass (class, _, _, _)) = getlabidClass class
  | getlabidLabclass (LabClassDots _)            = NONE


(* Extract the label from a labid *)

fun getLabelLabId (LabId (_, _, lab, _)) = L.singleton lab
  | getLabelLabId (LabIdDots _)          = L.empty


(* Extract the ident label from a labid *)

fun getLabelsIdPCon (PconBool (_, _, _, lab, _)) = SOME lab
  | getLabelsIdPCon (PconNil  (_, _, _, lab, _)) = SOME lab
  | getLabelsIdPCon (PconRef  (_, _, _, lab, _)) = SOME lab
  | getLabelsIdPCon PconDots                     = NONE

fun getLabelsIdIdent (Ident (_, _, _, lab, _)) = SOME lab
  | getLabelsIdIdent (IdentPcon pcon)          = getLabelsIdPCon pcon
  | getLabelsIdIdent IdentDots                 = NONE

fun getLabelsIdLabId (LabId (ident, _, lab, _)) =
    (case getLabelsIdIdent ident of
	 NONE      => NONE
       | SOME lab' => SOME (lab, lab'))
  | getLabelsIdLabId (LabIdDots _) = NONE


(* Get the type variables from some type related expressions *)

fun getTypeVarTypeVarSeq (TypeVarSeqOne (ty, reg, lab, nxt))   = [ty]
  | getTypeVarTypeVarSeq (TypeVarSeqEm (reg, lab, nxt))        = []
  | getTypeVarTypeVarSeq (TypeVarSeqSeq (tvl, regl, lab, nxt)) = getTypeVarLabTypeVarList tvl
  | getTypeVarTypeVarSeq (TypeVarSeqDots tvl)                  = map (fn x => x) tvl

and getTypeVarLabTypeVarList xs = foldr (fn (x, y) => (getTypeVarLabTypeVar x) @ y) [] xs

and getTypeVarLabTypeVar (LabTypeVar (tv, _, _, _)) = [tv]
  | getTypeVarLabTypeVar (LabTypeVarDots _)         = []

and getTypeVarLabType (LabType (t, _, _, _)) = getTypeVarType t
  | getTypeVarLabType (LabTypeDots _)        = []

and getTypeVarTypeRow (TypeRowOne (ty, _, lab, _))  = getTypeVarType ty
  | getTypeVarTypeRow (TypeRowEm (_, lab, _))       = []
  | getTypeVarTypeRow (TypeRowSeq (tyl, _, lab, _)) = foldr (fn (x, y) => (getTypeVarLabType x) @ y) [] tyl
  | getTypeVarTypeRow (TypeRowDots _)               = []

and getTypeVarTyField (TyField (_, lt, _, _, _)) = getTypeVarLabType lt
  | getTypeVarTyField (TyFieldDots p_)           = []

and getTypeVarType (TypeOneVar tv)                 = [tv]
  | getTypeVarType (TypeArrow (ty1, ty2, _, _, _)) = (getTypeVarLabType ty1) @ (getTypeVarLabType ty2)
  | getTypeVarType (TypeTuple (tyl, _, _, _))      = foldr (fn (x, y) => (getTypeVarLabType x) @ y) [] tyl
  | getTypeVarType (TypeRecord (trl, _, _, _, _))  = foldr (fn (x, y) => (getTypeVarTyField x) @ y) [] trl
  | getTypeVarType (TypeSlRec (trl, _, _, _))      = foldr (fn (x, y) => (getTypeVarTyField x) @ y) [] trl
  | getTypeVarType (TypeTyCon (ts, _, _, _, _))    = getTypeVarTypeRow ts
  | getTypeVarType (TypeParen (ty, _, _, _, _))    = getTypeVarLabType ty
  | getTypeVarType (TypeDots _)                    = raise EH.TODO (* TODO: do that for all the other dot nodes! *)

and getTypeVarConbind (ConBind _)                         = []
  | getTypeVarConbind (ConBindOf (id, ty, reg, lab, nxt)) = getTypeVarLabType ty
  | getTypeVarConbind (ConBindNoOf _)                     = []
  | getTypeVarConbind (ConBindDots _)                     = []

and getTypeVarConbindseq (ConBindSeq tycl)  = foldr (fn (x, y) => (getTypeVarConbind x) @ y) [] tycl
  | getTypeVarConbindseq (ConBindSeqDots _) = []

and getTypeVarLabExp (LabExp (e, _, _, _, _)) = getTypeVarExp e
  | getTypeVarLabExp (LabExpDots _)           = []

and getTypeVarExpField (ExpField (_, e, _, _, _, _)) = getTypeVarLabExp e
  | getTypeVarExpField (ExpFieldDots _)              = []

(*and getTypeVarExpRec (ExpRecSeq (erl, _, _, _)) = foldr (fn (x, y) => (getTypeVarExpField x) @ y) [] erl
  | getTypeVarExpRec (ExpRecSet erl)            = foldr (fn (x, y) => (getTypeVarExpField x) @ y) [] erl
  | getTypeVarExpRec (ExpRecDots _)             = []*)

and getTypeVarAtExp (AtExpId _)                     = []
  | getTypeVarAtExp (AtExpScon _)                   = []
  | getTypeVarAtExp (AtExpTuple (lel, _, _, _))     = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] lel
  | getTypeVarAtExp (AtExpRecord (erl, _, _, _, _)) = foldr (fn (x, y) => (getTypeVarExpField x) @ y) [] erl
  | getTypeVarAtExp (AtExpSlRec (erl, _, _, _))     = foldr (fn (x, y) => (getTypeVarExpField x) @ y) [] erl
  | getTypeVarAtExp (AtExpLet (ds, le, _, _, _))    = (getTypeVarDecs ds) @ (getTypeVarLabExp le)
  | getTypeVarAtExp (AtExpDLet (ds, seq, _, _, _))  = (getTypeVarDecs ds) @ (getTypeVarSeqExp seq)
  | getTypeVarAtExp (AtExpParen (le, _, _, _, _))   = getTypeVarLabExp le
  | getTypeVarAtExp (AtExpList (lel, _, _, _))      = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] lel
  | getTypeVarAtExp (AtExpProj _)                   = []
  | getTypeVarAtExp (AtExpSeq (seq, _, _, _))       = getTypeVarSeqExp seq
  | getTypeVarAtExp (AtExpQuote (quotes, _, _, _))  = getTypeVarQuotes quotes
  | getTypeVarAtExp (AtExpDots _)                   = []

and getTypeVarQuotes quotes = foldr (fn (quote, typeVars) => (getTypeVarQuote quote) @ typeVars) [] quotes

and getTypeVarQuote (Quote _)                  = []
  | getTypeVarQuote (Antiquote (exp, _, _, _)) = getTypeVarExp exp
  | getTypeVarQuote (QuoteDots _)              = []

and getTypeVarSeqExp (SeqExp (el, e, _, _, _, _))  = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] (el @ [e])
  | getTypeVarSeqExp (SeqExpSl (pl, e, _, _, _))   = getTypeVarLabExp e
  | getTypeVarSeqExp (SeqExpDots _)                = []

and getTypeVarExp (ExpAtExp ae)                      = getTypeVarAtExp ae
  | getTypeVarExp (ExpFn (m, _, _, _))               = getTypeVarMatch m
  | getTypeVarExp (ExpApp (le, ae, _, _, _, _, _))   = (getTypeVarExp le) @ (getTypeVarAtExp ae)
  | getTypeVarExp (ExpCase (le, m, _, _, _, _))      = (getTypeVarLabExp le) @ (getTypeVarMatch m)
  | getTypeVarExp (ExpConsList (_, e1, e2, _, _, _)) = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] [e1, e2]
  | getTypeVarExp (ExpOp (_, _, le1, le2, _, _, _))  = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] [le1, le2]
  | getTypeVarExp (ExpOr (le1, le2, _, _, _))        = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] [le1, le2]
  | getTypeVarExp (ExpAnd (le1, le2, _, _, _))       = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] [le1, le2]
  | getTypeVarExp (ExpTyped (le, lt, _, _, _))       = (getTypeVarLabExp le) @ (getTypeVarLabType lt)
  | getTypeVarExp (ExpIte (le1, le2, le3, _, _, _))  = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] [le1, le2, le3]
  | getTypeVarExp (ExpWhile (le1, le2, _, _, _, _))  = foldr (fn (x, y) => (getTypeVarLabExp x) @ y) [] [le1, le2]
  | getTypeVarExp (ExpRaise (e, _, _, _))            = getTypeVarLabExp e
  | getTypeVarExp (ExpHandle (e, m, _, _, _))        = (getTypeVarLabExp e) @ (getTypeVarMatch m)
  | getTypeVarExp (ExpDots _)                        = []

and getTypeVarMRule (Mrule (lp, le, _, _, _)) = (getTypeVarLabPat lp) @ (getTypeVarLabExp le)
  | getTypeVarMRule (MruleDots _)             = []

and getTypeVarMatch (Match (mrl, _, _)) = foldr (fn (x, y) => (getTypeVarMRule x) @ y) [] mrl
  | getTypeVarMatch (MatchDots _)       = []

and getTypeVarLabPat (LabPat (p, _, _, _, _)) = getTypeVarPat p
  | getTypeVarLabPat (LabPatDots _)           = []

and getTypeVarIdentTy (IdentTyId _)               = []
  | getTypeVarIdentTy (IdentTyTy (_, t, _, _, _)) = getTypeVarLabType t
  | getTypeVarIdentTy (IdentTyDots _)             = []

and getTypeVarLabIdTy (LabIdTy (id, _, _, _)) = getTypeVarIdentTy id
  | getTypeVarLabIdTy (LabIdTyDots _)         = []

and getTypeVarPatField (PatField (_, p, _, _, _, _)) = getTypeVarLabPat p
  | getTypeVarPatField (PatFieldId (id, _))          = getTypeVarIdentTy id
  | getTypeVarPatField (PatFieldAs (id, p, _, _, _)) = (getTypeVarLabIdTy id) @ (getTypeVarLabPat p)
  | getTypeVarPatField (PatFieldWild _)              = []
  | getTypeVarPatField (PatFieldDots _)              = []

and getTypeVarAtPat (AtPatWild _)                 = []
  | getTypeVarAtPat (AtPatId _)                   = []
  | getTypeVarAtPat (AtPatScon _)                 = []
  | getTypeVarAtPat (AtPatTuple (p, _, _, _))     = foldr (fn (x, y) => (getTypeVarLabPat x) @ y) [] p
  | getTypeVarAtPat (AtPatRecord (p, _, _, _, _)) = foldr (fn (x, y) => (getTypeVarPatField x) @ y) [] p
  | getTypeVarAtPat (AtPatParen (lp, _, _, _, _)) = getTypeVarLabPat lp
  | getTypeVarAtPat (AtPatList (xs, _, _, _))     = foldr (fn (x, y) => (getTypeVarLabPat x) @ y) [] xs
  | getTypeVarAtPat (AtPatOr (xs, _, _, _))       = foldr (fn (x, y) => (getTypeVarLabPat x) @ y) [] xs
  | getTypeVarAtPat (AtPatDots _)                 = []

and getTypeVarPat (PatAtPat ap)                      = getTypeVarAtPat ap
  | getTypeVarPat (PatApp (_, ap, _, _, _, _))       = getTypeVarAtPat ap
  | getTypeVarPat (PatConsList (_, p1, p2, _, _, _)) = foldr (fn (x, y) => (getTypeVarLabPat x) @ y) [] [p1, p2]
  | getTypeVarPat (PatOp (_, _, p1, p2, _, _, _))    = foldr (fn (x, y) => (getTypeVarLabPat x) @ y) [] [p1, p2]
  | getTypeVarPat (PatTyped (lp, lt, _, _, _))       = (getTypeVarLabPat lp) @ (getTypeVarLabType lt)
  | getTypeVarPat (PatAs (id, lp, _, _, _))          = (getTypeVarLabIdTy id) @ (getTypeVarLabPat lp)
  | getTypeVarPat (PatDots _)                        = []

and getTypeVarLabAtPat (LabAtPat (ap, _, _, _))   = getTypeVarAtPat ap
  | getTypeVarLabAtPat (LabAtPatDots _)           = []

and getTypeVarFMatch (FMatchId _)                         = []
  | getTypeVarFMatch (FMatchApp (fm, lap, _, _, _, _))    = (getTypeVarFMatch fm) @ (getTypeVarLabAtPat lap)
  | getTypeVarFMatch (FMatchSlApp (fm, lap, _))           = (getTypeVarFMatch fm) @ (getTypeVarLabAtPat lap)
  | getTypeVarFMatch (FMatchNoApp (fm, _))                = getTypeVarFMatch fm
  | getTypeVarFMatch FMatchDots                           = []

and getTypeVarDatName (DatName (tvs, _, _, _)) = getTypeVarTypeVarSeq tvs
  | getTypeVarDatName DatNameDots              = []

and getTypeVarLDatName (LDatName (tvs, _, _, _)) = getTypeVarTypeVarSeq tvs
  | getTypeVarLDatName LDatNameDots              = []

and getTypeVarLabFMatch (LabFMatch (fm, _, _, _))          = getTypeVarFMatch fm
  (*| getTypeVarLabFMatch (LabFMatchTy (fm, ty, _, _, _, _)) = (getTypeVarFMatch fm) @ (getTypeVarLabType ty)*)
  | getTypeVarLabFMatch (LabFMatchSl (fm, _))              = getTypeVarFMatch fm
  | getTypeVarLabFMatch LabFMatchDots                      = []

and getTypeVarFMatchTy (FMatchT fm)                  = getTypeVarLabFMatch fm
  | getTypeVarFMatchTy (FMatchTTy (fm, ty, _, _, _)) = (getTypeVarLabFMatch fm) @ (getTypeVarLabType ty)
  | getTypeVarFMatchTy FMatchTDots                   = []

and getTypeVarFValBindCore (FValBindCore (fm, le, _, _, _)) = (getTypeVarFMatchTy fm) @ (getTypeVarLabExp le)
  (*| getTypeVarFValBindCore (FVBCoreTy (lfm, ty, le, _, _, _, _)) = (getTypeVarLabFMatch lfm) @ (getTypeVarLabType ty) @ (getTypeVarLabExp le)*)
  | getTypeVarFValBindCore (FVBCoreDots _)                  = []

and getTypeVarFValBindOne (FValBindOne (fvbcl, _, _, _)) = foldr (fn (x, y) => (getTypeVarFValBindCore x) @ y) [] fvbcl
  | getTypeVarFValBindOne (FVBOneDots _)                 = []

and getTypeVarFValBind (FValBind (fvbol, _, _)) = foldr (fn (x, y) => (getTypeVarFValBindOne x) @ y) [] fvbol
  | getTypeVarFValBind (FValBindDots _)         = []

and getTypeVarExBind (ExBind _)                 = []
  | getTypeVarExBind (ExBindOf (_, t, _, l, _)) = getTypeVarLabType t
  | getTypeVarExBind (ExBindEq _)               = []
  | getTypeVarExBind (ExBindNo _)               = []
  | getTypeVarExBind (ExBindDots _)             = []

and getTypeVarExBindSeq (ExBindSeq (ebl, _, _)) = foldr (fn (x, y) => (getTypeVarExBind x) @ y) [] ebl
  | getTypeVarExBindSeq (ExBindSeqDots pl)      = []

and getTypeVarValBindCore (ValBindCore (p, e, _, _, _)) = (getTypeVarLabPat p) @ (getTypeVarLabExp e)
  | getTypeVarValBindCore (ValBindCoreDots pl)          = []

and getTypeVarValBindSeq (ValBindSeq (xs, _, _)) = foldr (fn (x, y) => (getTypeVarValBindCore x) @ y) [] xs
  | getTypeVarValBindSeq (ValBindSeqDots _)      = []

and getTypeVarValBind (ValBind vbs)               = getTypeVarValBindSeq vbs
  | getTypeVarValBind (ValBindRec (vbs, _, _, _)) = getTypeVarValBindSeq vbs
  | getTypeVarValBind (ValBindDots _)             = []

and getTypeVarDec (DecVal       _)         = [] (*getTypeVarValBind vb*) (* it depends on the expansivness of the nested expressions *)
  | getTypeVarDec (DecDatType   _)         = []
  | getTypeVarDec (DecDatWith   _)         = []
  | getTypeVarDec (DecDatRep    _)         = []
  | getTypeVarDec (DecFVal      _)         = []
  | getTypeVarDec (DecType      _)         = []
  | getTypeVarDec (DecEx (ebs, _, _))      = getTypeVarExBindSeq ebs
  | getTypeVarDec (DecOpen (ids, _, _, _)) = [] (* TODO:  oooooooooooh that's tough! - do we still need these getTypeVar anyway??? *)
  | getTypeVarDec (DecLocal    _)          = []
  | getTypeVarDec (DecAbsType  _)          = []
  | getTypeVarDec (DecAbsWith  _)          = []
  | getTypeVarDec (DecInfix    _)          = []
  | getTypeVarDec (DecInfixr   _)          = []
  | getTypeVarDec (DecNonfix   _)          = []
  | getTypeVarDec (DecOverload _)          = []
  | getTypeVarDec (DecClass    _)          = []
  | getTypeVarDec (DecDots     _)          = []

and getTypeVarDecs (Decs (dl, _)) = foldr (fn (x, y) => (getTypeVarDec x) @ y) [] dl
  | getTypeVarDecs (DecsDots pl)  = []

and getTypeVarValDescOne (ValDescOne (_, t, _, _, _)) = getTypeVarLabType t
  | getTypeVarValDescOne (ValDescOneDots pl)          = []

and getTypeVarValDesc (ValDesc (vdl, _, _)) = foldr (fn (x, y) => (getTypeVarValDescOne x) @ y) [] vdl
  | getTypeVarValDesc (ValDescDots pl)      = []

and getTypeVarStrDec (StrDec (xs, _, _)) = getTypeVarStrDecOneList xs
  | getTypeVarStrDec (StrDecDots pl)     = []

and getTypeVarStrDecOneList xs = foldr (fn (x, y) => (getTypeVarStrDecOne x) @ y) [] xs

and getTypeVarStrDecOne (StrDecOneDec  d) = getTypeVarDecs d
  | getTypeVarStrDecOne (StrDecOneStr  _) = [] (* check that *)
  | getTypeVarStrDecOne (StrDecOneLoc  _) = []
  | getTypeVarStrDecOne (StrDecOneFun  _) = []
  | getTypeVarStrDecOne (StrDecOneDots _) = []

and getTypeVarExcDescOne (ExcDescOne _)               = []
  | getTypeVarExcDescOne (ExcDescOf (_, t, _, _, _) ) = getTypeVarLabType t
  | getTypeVarExcDescOne (ExcDescOneDots pl)          = []

and getTypeVarExcDesc (ExcDesc (xs, _, _)) = foldr (fn (x, y) => (getTypeVarExcDescOne x) @ y) [] xs
  | getTypeVarExcDesc (ExcDescDots pl)     = []

and getTypeVarTypDescOne (TypDescOne (dn, _, _)) = getTypeVarDatName dn
  | getTypeVarTypDescOne (TypDescOneDots pl)     = []

and getTypeVarTypDesc (TypDesc (xs, _, _)) = foldr (fn (x, y) => (getTypeVarTypDescOne x) @ y) [] xs
  | getTypeVarTypDesc (TypDescDots pl)     = []

and getTypeVarTdrDescOne (TdrDescOne (dn, ty, _, _, _)) = (getTypeVarDatName dn) @ (getTypeVarLabType ty)
  | getTypeVarTdrDescOne (TdrDescOneDots pl)            = []

and getTypeVarTdrDesc (TdrDesc (xs, _, _)) = foldr (fn (x, y) => (getTypeVarTdrDescOne x) @ y) [] xs
  | getTypeVarTdrDesc (TdrDescDots pl)     = []

and getTypeVarConDescOne (ConDescOneId _)                = []
  | getTypeVarConDescOne (ConDescOneOf (_, ty, _, _, _)) = getTypeVarLabType ty
  | getTypeVarConDescOne (ConDescOneNoOf _)              = []
  | getTypeVarConDescOne (ConDescOneDots pl)             = []

and getTypeVarConDesc (ConDesc (xs, _, _)) = foldr (fn (x, y) => (getTypeVarConDescOne x) @ y) [] xs
  | getTypeVarConDesc (ConDescDots pl)     = []

and getTypeVarDatDescOne (DatDescOne (dn, cd, _, _, _)) = (getTypeVarDatName dn) @ (getTypeVarConDesc cd)
  | getTypeVarDatDescOne (DatDescOneDots pl)            = []

and getTypeVarDatDesc (DatDesc (xs, _, _)) = foldr (fn (x, y) => (getTypeVarDatDescOne x) @ y) [] xs
  | getTypeVarDatDesc (DatDescDots pl)     = []

and getTypeVarSpecOne (SpecValue (vd, _, _, _)) = []
  | getTypeVarSpecOne (SpecType (tp, _, _, _)) = getTypeVarTypDesc tp
  | getTypeVarSpecOne (SpecEqtype (tp, _, _, _)) = getTypeVarTypDesc tp
  | getTypeVarSpecOne (SpecException (ex, _, _, _)) = getTypeVarExcDesc ex
  | getTypeVarSpecOne (SpecTdr (tp, _, _, _)) = getTypeVarTdrDesc tp
  | getTypeVarSpecOne (SpecDat (dd, _, _, _)) = getTypeVarDatDesc dd
  | getTypeVarSpecOne (SpecStr (sd, _, _, _)) = [] (* What should be here? *)
  | getTypeVarSpecOne (SpecInc (si, _, _, _)) = []
  | getTypeVarSpecOne (SpecIsi _)             = []
  | getTypeVarSpecOne (SpecRep _)             = []
  | getTypeVarSpecOne (SpecSha _)             = []
  | getTypeVarSpecOne (SpecSsi _)             = []
  | getTypeVarSpecOne (SpecOneDots _)         = []

and getTypeVarSpec (Spec (xs, _)) = foldr (fn (x, y) => (getTypeVarSpecOne x) @ y) [] xs
  | getTypeVarSpec (SpecDots pl)  = []

and getTypeVarLabSigExp (LabSigExp (e, _, _, _, _)) = getTypeVarSigExp e
  | getTypeVarLabSigExp (LabSigExpDots pl)          = []

and getTypeVarSigExp (SigExpBasic (sp, _, _, _)) = getTypeVarSpec sp
  | getTypeVarSigExp (SigExpId _)                = []
  | getTypeVarSigExp (SigExpRea _)               = []
  | getTypeVarSigExp (SigExpDots pl)             = []

and getTypeVarLabStrExp (LabStrExp (e, _, _, _, _)) = getTypeVarStrExp e
  | getTypeVarLabStrExp (LabStrExpDots pl)          = []

and getTypeVarStrExp (StrExpBasic (sd, _, _, _)) = getTypeVarStrDec sd
  | getTypeVarStrExp (StrExpId    _)             = []
  | getTypeVarStrExp (StrExpOp    _)             = []
  | getTypeVarStrExp (StrExpTr    _)             = []
  | getTypeVarStrExp (StrExpFExp  _)             = []
  | getTypeVarStrExp (StrExpFDec  _)             = []
  | getTypeVarStrExp (StrExpLocal _)             = []
  | getTypeVarStrExp (StrExpDots pl)             = []

and getTypeVarSigBindOne (SigBindOne (id, se, _, _, _)) = getTypeVarLabSigExp se
  | getTypeVarSigBindOne (SigBindOneDots _)             = []

and getTypeVarSigBind (SigBind (xs, _, _)) = foldr (fn (x, y) => (getTypeVarSigBindOne x) @ y) [] xs
  | getTypeVarSigBind (SigBindDots _)      = []

and getTypeVarSigDec (SigDec (sb, _, _)) = getTypeVarSigBind sb
  | getTypeVarSigDec (SigDecDots _)      = []

and getTypeVarFunBindOne (FunBindO   (_, _, si, se, _, _, _))      = (getTypeVarLabSigExp si) @ (getTypeVarLabStrExp se)
  | getTypeVarFunBindOne (FunBindOO  (_, _, si, si', se, _, _, _)) = (getTypeVarLabSigExp si) @ (getTypeVarLabSigExp si') @ (getTypeVarLabStrExp se)
  | getTypeVarFunBindOne (FunBindOT  (_, _, si, si', se, _, _, _)) = (getTypeVarLabSigExp si) @ (getTypeVarLabSigExp si') @ (getTypeVarLabStrExp se)
  | getTypeVarFunBindOne (FunBindOS  (_, _, se, _, _, _))          = (getTypeVarLabStrExp se)
  | getTypeVarFunBindOne (FunBindOSO (_, _, si, se, _, _, _))      = (getTypeVarLabSigExp si) @ (getTypeVarLabStrExp se)
  | getTypeVarFunBindOne (FunBindOST (_, _, si, se, _, _, _))      = (getTypeVarLabSigExp si) @ (getTypeVarLabStrExp se)
  | getTypeVarFunBindOne (FunBindODots _)                          = []

and getTypeVarFunBind (FunBind (xs, _, _)) = foldr (fn (x, y) => (getTypeVarFunBindOne x) @ y) [] xs
  | getTypeVarFunBind (FunBindDots _)      = []

(*and getTypeVarFunDec (FunDec (sb, _, _, _)) = getTypeVarFunBind sb
  | getTypeVarFunDec (FunDecDots _)         = []*)

and getTypeVarATopDec (ATopDecStr s)  = getTypeVarStrDec s
  | getTypeVarATopDec (ATopDecSig s)  = getTypeVarSigDec s
  (*| getTypeVarATopDec (TopDecOneFun f)  = getTypeVarFunDec f*)
  | getTypeVarATopDec (ATopDecDots _) = []

and getTypeVarTopDecOne (TopDecOneTes _)      = []
  | getTypeVarTopDecOne (TopDecOneDec (x, _)) = getTypeVarATopDec x
  | getTypeVarTopDecOne (TopDecOneDots _)     = []

and getTypeVarTopDec (TopDec xs)    = foldr (fn (x, y) => (getTypeVarTopDecOne x) @ y) [] xs
  | getTypeVarTopDec (TopDecDots _) = []

and getTypeVarProgOne (ProgOneDec td)  = getTypeVarTopDec td
  | getTypeVarProgOne (ProgOneExp   _) = []
  | getTypeVarProgOne (ProgOneParse _) = []
  | getTypeVarProgOne (ProgOneFile  _) = []
  | getTypeVarProgOne (ProgOneDots  _) = []

and getTypeVarProg (Prog xs)     = foldr (fn (x, y) => (getTypeVarProgOne x) @ y) [] xs
  | getTypeVarProg (ProgDots _)  = []

and getTypeVarProgs (Progs xs) = foldr (fn ((x, _, _, _), y) => (getTypeVarProg x) @ y) [] xs


(* inclusion type variable *)



(*
fun getstregtypeVarlist [] = []
  | getstregtypeVarlist ((TypeVar (st, n, reg, lab, nxt))::xs) = (st, reg)::(getstregtypeVarlist xs)

fun typeVarin _ [] = false
  | typeVarin (x as (TypeVar (_, n, reg, _, _))) ((TypeVar (_, m, reg', _, _))::xs) =
      if n = m then true else typeVarin x xs

fun typeVarnotincl [] ys      = []
  | typeVarnotincl (x::xs) ys = if (typeVarin x ys) then typeVarnotincl xs ys else x::(typeVarnotincl xs ys)
*)


(* id in *)


(*
fun typeVarin _ [] = false
  | typeVarin (x as (TypeVar (_, n, _, _, _))) ((TypeVar (_, m, _, _, _))::xs) =
      if n = m then true else typeVarin x xs
*)


(**)


(*
val getmultiocctypeVar = fn tyl =>
  let
  fun find st [] = ([], [])
    | find st ((x as (TypeVar (s, _, reg, _, _)))::xs) =
        let
        val (fd, ys) = find st xs
        in if st = s then (reg::fd, ys) else (fd, x::ys)
        end
  fun f [] = []
    | f ((TypeVar (st, _, reg, _, _))::xs) =
        let
        val (regl, ys) = find st xs
        in (st, reg::regl)::(f ys)
        end
  fun g [] = []
    | g ((_, [])::xs) = g xs
    | g ((_, [_])::xs) = g xs
    | g ((st, regl)::xs) = (st, regl)::(g xs)
  in g (f tyl)
  end
*)


(* return the last label associated a type *)
(* we shouldn't need this one anymore *)


(*
fun getlabtypeVar (TypeVar (st, n, reg, lab, nxt))    = lab

fun getlabty (TypeOneVar tv)                        = getlabtypeVar tv
  | getlabty (TypeUnit (reg, lab, nxt))             = lab
  | getlabty (TypeArrow (ty1, ty2, reg, lab, nxt))  = lab
  | getlabty (TypeTuple (tyl, regl, lab, nxt))      = lab
  | getlabty (TypeTyCon (ts, tn, reg, lab, nxt)) = lab
  | getlabty (TypeParen (ty, r1, r2, lab, nxt))     = lab
*)


(* return the list of the constructors in a datbindseq *)


(*
fun getiddatbindseqtc (ConBind (Ident (st, v, reg, _, _))) =
      [(st, reg)]
  | getiddatbindseqtc (ConBindOf (Ident (st, v, reg, _, _), _, _, _, _)) =
      [(st, reg)]

fun getiddatbindseqtcs (ConBindSeq tcl) =
      T.union (map getiddatbindseqtc tcl)

fun getiddatbindseqdb (DatBind (_, _, tcs, _, _, _)) =
      getiddatbindseqtcs tcs

fun getiddatbindseq (DatBindSeq (dbl, _, _)) =
      T.union (map getiddatbindseqdb dbl)

(* we sould use lkup instead of the next one *)
fun multiiddatbind [] = []
  | multiiddatbind ((id, reg) :: xs) =
      let
      fun f [] = ([], [])
        | f ((x as (id', reg')) :: xs) =
            let
            val (regs, xss) = f xs
            in if id = id' then (reg' :: regs, xss) else (regs, x :: xss)
            end
      val (regs, xss) = f xs
      in if regs = [] then multiiddatbind xs else (id, reg :: regs) :: (multiiddatbind xss)
      end
*)


(* return the list of the type names in a datbindseq *)


(*
fun gettndatbindseqtn (TyCon (st, v, reg, _, _)) = [(st, reg)]

fun gettndatbindseqdn (DatName (_, tn)) = gettndatbindseqtn tn

fun gettndatbindseqdb (DatBind (dn, _, _, _, _)) = gettndatbindseqdn dn

fun gettndatbindseq (DatBindSeq (dbl, _, _)) =
      T.union (map gettndatbindseqdb dbl)

(* we sould use lkup instead of the next one *)
fun multitndatbind [] = []
  | multitndatbind ((st, reg) :: xs) =
      let
      fun f [] = ([], [])
        | f ((x as (st', reg')) :: xs) =
            let
            val (regs, xss) = f xs
            in if st = st' then (reg' :: regs, xss) else (regs, x :: xss)
            end
      val (regs, xss) = f xs
      in if regs = [] then multitndatbind xs else (st, reg :: regs) :: (multitndatbind xss)
      end
*)


(* return the next label of a term *)


fun getSconNext (SconInt    (_, _, _, _, n))         = SOME n
  | getSconNext (SconWord   (_, _, _, _, n))         = SOME n
  | getSconNext (SconReal   (_, _, _, _, n))         = SOME n
  | getSconNext (SconString (_, _, _, _, n))         = SOME n
  | getSconNext (SconChar   (_, _, _, _, n))         = SOME n
  | getSconNext SconDots                             = NONE

and getPconNext (PconBool (_, _, _, _, n))           = SOME n
  | getPconNext (PconNil  (_, _, _, _, n))           = SOME n
  | getPconNext (PconRef  (_, _, _, _, n))           = SOME n
  | getPconNext PconDots                             = NONE

and getStrIdNext (StrId (_, _, _, _, n))             = SOME n
  | getStrIdNext StrIdDots                           = NONE

and getSigIdNext (SigId (_, _, _, _, n))             = SOME n
  | getSigIdNext SigIdDots                           = NONE

and getFunIdNext (FunId (_, _, _, _, n))             = SOME n
  | getFunIdNext FunIdDots                           = NONE

and getLabIdNext (LabId (_, _, _, n))                = SOME n
  | getLabIdNext (LabIdDots pl)                      = getPartListNext pl

and getIdentNext (Ident (_, _, _, _, n))             = SOME n
  | getIdentNext (IdentPcon pc)                      = getPconNext pc
  | getIdentNext IdentDots                           = NONE

(*and getIdentListNext xs                              = getIdentNext (List.last xs) handle Empty => raise DeadBranch*)

and getLabClassNext (LabClass (_, _, _, n))          = SOME n
  | getLabClassNext (LabClassDots pl)                = getPartListNext pl

and getClassNext (Class (_, _, _, _, n))             = SOME n
  | getClassNext ClassDots                           = NONE

and getLabTyClassNext (LabTyClass (_, _, _, n))      = SOME n
  | getLabTyClassNext (LabTyClassDots pl)            = getPartListNext pl

and getLongIdNext (LongIdQual (_, _, _, _, n))       = SOME n
  | getLongIdNext (LongIdId id)                      = getIdentNext id
  | getLongIdNext (LongIdDots pl)                    = getPartListNext pl

and getLongStrIdNext (LongStrIdQual (_, _, _, _, n)) = SOME n
  | getLongStrIdNext (LongStrIdId id)                = getStrIdNext id
  | getLongStrIdNext (LongStrIdDots pl)              = getPartListNext pl

and getLongTyConNext (LongTyConQual (_, _, _, _, n)) = SOME n
  | getLongTyConNext (LongTyConId tc)                = getTyConNext tc
  | getLongTyConNext (LongTyConDots pl)              = getPartListNext pl

and getTyLabNext (TyLab (_, _, _, n))                = SOME n
  | getTyLabNext TyLabDots                           = NONE

and getTyConNext (TyCon (_, _, _, _, n))             = SOME n
  | getTyConNext TyConDots                           = NONE

and getLabTypeVarNext (LabTypeVar (_, _, _, n))          = SOME n
  | getLabTypeVarNext (LabTypeVarDots tvl)               = getTypeVarListNext tvl

and getTypeVarNext (TypeVar (_, _, _, _, n))         = SOME n
  | getTypeVarNext TypeVarNext                       = NONE

and getTypeVarListNext []                            = NONE
  | getTypeVarListNext [tv]                          = getTypeVarNext tv
  | getTypeVarListNext (_ :: xs)                     = getTypeVarListNext xs

and getTypeVarSeqNext (TypeVarSeqOne (_, _, _, n))       = SOME n
  | getTypeVarSeqNext (TypeVarSeqEm (_, _, n))           = SOME n
  | getTypeVarSeqNext (TypeVarSeqSeq (_, _, _, n))       = SOME n
  | getTypeVarSeqNext (TypeVarSeqDots tvl)               = getTypeVarListNext tvl

and getLabTypeNext (LabType (_, _, _, n))            = SOME n
  | getLabTypeNext (LabTypeDots pl)                  = getPartListNext pl

and getTyFieldNext (TyField (_, _, _, _, n))             = SOME n
  | getTyFieldNext (TyFieldDots pl)                      = getPartListNext pl

and getTypeNext (TypeOneVar tv)                      = getTypeVarNext tv
  | getTypeNext (TypeArrow (_, _, _, _, n))          = SOME n
  | getTypeNext (TypeTuple (_, _, _, n))             = SOME n
  | getTypeNext (TypeRecord (_, _, _, _, n))         = SOME n
  | getTypeNext (TypeSlRec (_, _, _, n))             = SOME n
  | getTypeNext (TypeTyCon (_, _, _, _, n))          = SOME n
  | getTypeNext (TypeParen (_, _, _, _, n))          = SOME n
  | getTypeNext (TypeDots pl)                        = getPartListNext pl

and getTypeRowNext (TypeRowOne (_, _, _, n))    = SOME n
  | getTypeRowNext (TypeRowEm (_, _, n))        = SOME n
  | getTypeRowNext (TypeRowSeq (_, _, _, n))     = SOME n
  | getTypeRowNext (TypeRowDots pl)             = getPartListNext pl

and getConBindNext (ConBind (_, n))                  = SOME n
  | getConBindNext (ConBindOf (_, _, _, _, n))       = SOME n
  | getConBindNext (ConBindNoOf (_, n))              = SOME n
  | getConBindNext (ConBindDots pl)                  = getPartListNext pl

and getValBindCoreNext (ValBindCore (_, _, _, _, n)) = SOME n
  | getValBindCoreNext (ValBindCoreDots pl)          = getPartListNext pl

and getValBindSeqNext (ValBindSeq (_, _, n))         = SOME n
  | getValBindSeqNext (ValBindSeqDots pl)            = getPartListNext pl

and getValBindNext (ValBindRec (vbs, _, _, n))       = SOME n
  | getValBindNext (ValBind vbs)                     = getValBindSeqNext vbs
  | getValBindNext (ValBindDots pl)                  = getPartListNext pl

and getDatNameNext (DatName (_, _, _, n))            = SOME n
  | getDatNameNext DatNameDots                       = NONE

and getLDatNameNext (LDatName (_, _, _, n))          = SOME n
  | getLDatNameNext LDatNameDots                     = NONE

and getLTReaDOneNext (LTReaDOne (_, _, _, _, n))     = SOME n
  | getLTReaDOneNext (LTReaDOneDots pl)              = getPartListNext pl

and getDatBindNext (DatBind ( _, _, _, _, n))        = SOME n
  | getDatBindNext (DatBindDots pl)                  = getPartListNext pl

and getDatBindSeqNext (DatBindSeq (_, _, n))         = SOME n
  | getDatBindSeqNext (DatBindSeqDots pl)            = getPartListNext pl

and getLabAtPatNext (LabAtPat (_, _, _, n))          = SOME n
  | getLabAtPatNext (LabAtPatDots pl)                = getPartListNext pl

and getFMatchNext (FMatchId (id, _, _))              = getIdentNext id
  | getFMatchNext (FMatchApp (_, _, _, _, _, n))     = SOME n
  | getFMatchNext (FMatchSlApp (_, _, n))            = SOME n
  | getFMatchNext (FMatchNoApp (_, n))               = SOME n
  | getFMatchNext FMatchDots                         = NONE

and getLabFMatchNext (LabFMatch (_, _, _, n))        = SOME n
  (*| getLabFMatchNext (LabFMatchTy (_, _, _, _, _, n))= SOME n*)
  | getLabFMatchNext (LabFMatchSl (_, n))            = SOME n
  | getLabFMatchNext LabFMatchDots                   = NONE

and getFMatchTyNext (FMatchT fm)                     = getLabFMatchNext fm
  | getFMatchTyNext (FMatchTTy (_, _, _, _, n))      = SOME n
  | getFMatchTyNext FMarchTDots                      = NONE

and getFVBCoreNext (FValBindCore (_, _, _, _, n))    = SOME n
  (*| getFVBCoreNext (FVBCoreTy (_, _, _, _, _, _, n)) = SOME n*)
  | getFVBCoreNext (FVBCoreDots pl)                  = getPartListNext pl

and getFVBOneNext (FValBindOne (_, _, _, n))         = SOME n
  | getFVBOneNext (FVBOneDots pl)                    = getPartListNext pl

and getFValBindNext (FValBind (_, _, n))             = SOME n
  | getFValBindNext (FValBindDots pl)                = getPartListNext pl

and getTypBindNext (TypBind (_, _, _, _, n))         = SOME n
  | getTypBindNext (TypBindDots pl)                  = getPartListNext pl

and getTypBindSeqNext (TypBindSeq (_, _, n))         = SOME n
  | getTypBindSeqNext (TypBindSeqDots pl)            = getPartListNext pl

and getExBindNext (ExBind (_, _, n))                 = SOME n
  | getExBindNext (ExBindOf (_, _, _, _, n))         = SOME n
  | getExBindNext (ExBindEq (_, _, _, _, n))         = SOME n
  | getExBindNext (ExBindNo (_, n))                  = SOME n
  | getExBindNext (ExBindDots pl)                    = getPartListNext pl

and getExBindSeqNext (ExBindSeq (_, _, n))           = SOME n
  | getExBindSeqNext (ExBindSeqDots pl)              = getPartListNext pl

and getLongStrSeqNext (LongStrSeq (_, n))            = SOME n
  | getLongStrSeqNext (LongStrSeqDots pl)            = getPartListNext pl

and getIdentSeqNext (IdentSeq (_, n))                = SOME n
  | getIdentSeqNext (IdentSeqDots pl)                = getPartListNext pl

and getDecNext (DecVal (_, _, _, n))                 = SOME n
  | getDecNext (DecFVal (_, _, _, n))                = SOME n
  | getDecNext (DecDatType (_, _, n))                = SOME n
  | getDecNext (DecDatWith (_, _, _, _, n))          = SOME n
  | getDecNext (DecDatRep (_, _, _, _, n))           = SOME n
  | getDecNext (DecType (_, _, n))                   = SOME n
  | getDecNext (DecEx (_, _, n))                     = SOME n
  | getDecNext (DecOpen (_, _, _, n))                = SOME n
  | getDecNext (DecLocal (_, _, _, _, n))            = SOME n
  | getDecNext (DecAbsType (_, _, _, _, n))          = SOME n
  | getDecNext (DecAbsWith (_, _, _, _, _, n))       = SOME n
  | getDecNext (DecInfix  (_, _, _, _, n))           = SOME n
  | getDecNext (DecInfixr (_, _, _, _, n))           = SOME n
  | getDecNext (DecNonfix (_, _, _, n))              = SOME n
  | getDecNext (DecOverload (_, _, _, _, _, _, n))   = SOME n
  | getDecNext (DecClass (_, _, _, _, n))            = SOME n
  | getDecNext (DecDots pl)                          = getPartListNext pl

and getLabExpNext (LabExp (_, _, _, _, n))           = SOME n
  | getLabExpNext (LabExpDots pl)                    = getPartListNext pl

and getExpFieldNext (ExpField (_, _, _, _, _, n))        = SOME n
  | getExpFieldNext (ExpFieldDots pl)                    = getPartListNext pl

and getAtExpNext (AtExpId id)                        = getLongIdNext id
  | getAtExpNext (AtExpScon sc)                      = getSconNext sc
  | getAtExpNext (AtExpTuple (_, _, _, n))           = SOME n
  | getAtExpNext (AtExpRecord (_, _, _, _, n))       = SOME n
  | getAtExpNext (AtExpSlRec (_, _, _, n))           = SOME n
  | getAtExpNext (AtExpLet (_, _, _, _, n))          = SOME n
  | getAtExpNext (AtExpDLet (_, _, _, _, n))         = SOME n
  | getAtExpNext (AtExpParen (_, _, _, _, n))        = SOME n
  | getAtExpNext (AtExpList (_, _, _, n))            = SOME n
  | getAtExpNext (AtExpProj (_, _, _, _, n))         = SOME n
  | getAtExpNext (AtExpSeq (_, _, _, n))             = SOME n
  | getAtExpNext (AtExpQuote (_, _, _, n))           = SOME n
  | getAtExpNext (AtExpDots pl)                      = getPartListNext pl

and getSeqExpNext (SeqExp (_, _, _, _, _, n))        = SOME n
  | getSeqExpNext (SeqExpSl (_, _, _, _, n))         = SOME n
  | getSeqExpNext (SeqExpDots pl)                    = getPartListNext pl

and getExpNext (ExpAtExp atexp)                      = getAtExpNext atexp
  | getExpNext (ExpFn (_, _, _, n))                  = SOME n
  | getExpNext (ExpApp (_, _, _, _, _, _, n))        = SOME n
  | getExpNext (ExpCase (_, _, _, _, _, n))          = SOME n
  | getExpNext (ExpConsList (_, _, _, _, _, n))      = SOME n
  | getExpNext (ExpOp (_, _, _, _, _, _, n))         = SOME n
  | getExpNext (ExpOr (_, _, _, _, n))               = SOME n
  | getExpNext (ExpAnd (_, _, _, _, n))              = SOME n
  | getExpNext (ExpTyped (_, _, _, _, n))            = SOME n
  | getExpNext (ExpIte (_, _, _, _, _, n))           = SOME n
  | getExpNext (ExpWhile (_, _, _, _, _, n))         = SOME n
  | getExpNext (ExpRaise (_, _, _, n))               = SOME n
  | getExpNext (ExpHandle (_, _, _, _, n))           = SOME n
  | getExpNext (ExpDots pl)                          = getPartListNext pl

and getQuoteNext (Quote (_, _, _, n))                = SOME n
  | getQuoteNext (Antiquote (_, _, _, n))            = SOME n
  | getQuoteNext (QuoteDots pl)                      = getPartListNext pl

and getMruleNext (Mrule (_, _, _, _, n))             = SOME n
  | getMruleNext (MruleDots pl)                      = getPartListNext pl

and getLabPatNext (LabPat (_, _, _, _, n))           = SOME n
  | getLabPatNext (LabPatDots pl)                    = getPartListNext pl

and getLabIdTyNext (LabIdTy (_, _, _, n))            = SOME n
  | getLabIdTyNext (LabIdTyDots pl)                  = getPartListNext pl

and getPatFieldNext (PatField (_, _, _, _, _, n))        = SOME n
  | getPatFieldNext (PatFieldId (_, n))                  = SOME n
  | getPatFieldNext (PatFieldAs (_, _, _, _, n))         = SOME n
  | getPatFieldNext (PatFieldWild (_, _, n))             = SOME n
  | getPatFieldNext (PatFieldDots pl)                    = getPartListNext pl

and getAtPatNext (AtPatWild (_, n))                  = SOME n
  | getAtPatNext (AtPatId id)                        = getLongIdNext id
  | getAtPatNext (AtPatScon sc)                      = getSconNext sc
  | getAtPatNext (AtPatTuple (_, _, _, n))           = SOME n
  | getAtPatNext (AtPatRecord (_, _, _, _, n))       = SOME n
  | getAtPatNext (AtPatParen (_, _, _, _, n))        = SOME n
  | getAtPatNext (AtPatList (_, _, _, n))            = SOME n
  | getAtPatNext (AtPatOr (_, _, _, n))              = SOME n
  | getAtPatNext (AtPatDots pl)                      = getPartListNext pl

and getIdentTyNext (IdentTyId id)                    = getIdentNext id
  | getIdentTyNext (IdentTyTy (_, _, _, _, n))       = SOME n
  | getIdentTyNext (IdentTyDots pl)                  = getPartListNext pl

and getPatNext (PatAtPat atpat)                      = getAtPatNext atpat
  | getPatNext (PatApp (_, _, _, _, _, n))           = SOME n
  | getPatNext (PatConsList (_, _, _, _, _, n))      = SOME n
  | getPatNext (PatOp (_, _, _, _, _, _, n))         = SOME n
  | getPatNext (PatTyped (_, _, _, _, n))            = SOME n
  | getPatNext (PatAs (_, _, _, _, n))               = SOME n
  | getPatNext (PatDots pl)                          = getPartListNext pl

and getPartListNext []                               = NONE
  | getPartListNext [x]                              = getPartNext x
  | getPartListNext (_ :: xs)                        = getPartListNext xs

and getPartNext (PartExp   e)                        = getExpNext       e
  | getPartNext (PartDec   d)                        = getDecNext       d
  | getPartNext (PartType  t)                        = getTypeNext      t
  | getPartNext (PartSeq   s)                        = getTypeRowNext   s
  | getPartNext (PartPat   p)                        = getPatNext       p
  | getPartNext (PartIdTy  i)                        = getIdentTyNext   i
  | getPartNext (PartTyCon t)                        = getLongTyConNext t
  | getPartNext (PartSpec  s)                        = getSpecOneNext   s
  | getPartNext (PartSige  s)                        = getSigExpNext    s
  | getPartNext (PartStre  s)                        = getStrExpNext    s
  (*| getPartNext (PartFund  f)                        = getFunDecNext    f*)
  | getPartNext (PartSigd  s)                        = getSigDecNext    s
  | getPartNext (PartStrd  s)                        = getStrDecOneNext s
  | getPartNext (PartLgid  i)                        = getLongIdNext    i
  | getPartNext (PartLgsid i)                        = getLongStrIdNext i
  | getPartNext (PartSigid i)                        = getSigIdNext     i
  | getPartNext (PartTes   t)                        = getSmlTesNext    t
  | getPartNext (PartClass c)                        = getClassNext     c

and getDecListNext []                                = NONE
  | getDecListNext [x]                               = getDecNext x
  | getDecListNext (_ :: xs)                         = getDecListNext xs

and getDecsNext (Decs (_, n))                        = SOME n
  | getDecsNext (DecsDots pl)                        = getPartListNext pl

(*and getFunDecNext (FunDec (_, _, _, n))              = SOME n
  | getFunDecNext (FunDecDots pl)                    = getPartListNext pl*)

and getSigDecNext (SigDec (_, _, n))                 = SOME n
  | getSigDecNext (SigDecDots pl)                    = getPartListNext pl

and getStrDecNext (StrDec (_, _, n))                 = SOME n
  | getStrDecNext (StrDecDots pl)                    = getPartListNext pl

and getStrDecOneListNext []                          = NONE
  | getStrDecOneListNext [x]                         = getStrDecOneNext x
  | getStrDecOneListNext (_ :: xs)                   = getStrDecOneListNext xs

and getStrDecOneNext (StrDecOneDec d)                = getDecsNext d
  | getStrDecOneNext (StrDecOneStr (_, _, n))        = SOME n
  | getStrDecOneNext (StrDecOneLoc (_, _, _, _, n))  = SOME n
  | getStrDecOneNext (StrDecOneFun (_, _, _, n))     = SOME n
  | getStrDecOneNext (StrDecOneDots pl)              = getPartListNext pl

and getStrBindNext (StrBind (xs, _, n))              = SOME n
  | getStrBindNext (StrBindDots pl)                  = getPartListNext pl

and getStrBONext (StrBindOneOp (_, _, _, _, _, n))   = SOME n
  | getStrBONext (StrBindOneTr (_, _, _, _, _, n))   = SOME n
  | getStrBONext (StrBindOne (_, _ , _, _, n))       = SOME n
  | getStrBONext (StrBindOneDots pl)                 = getPartListNext pl

and getSigBONext (SigBindOne (_, _ , _, _, n))       = SOME n
  | getSigBONext (SigBindOneDots pl)                 = getPartListNext pl

and getFunBONext (FunBindO (_, _, _, _, _, _, n))     = SOME n
  | getFunBONext (FunBindOO (_, _, _, _, _, _, _, n)) = SOME n
  | getFunBONext (FunBindOT (_, _, _, _, _, _, _, n)) = SOME n
  | getFunBONext (FunBindOS (_, _, _, _, _, n))       = SOME n
  | getFunBONext (FunBindOSO (_, _, _, _, _, _, n))   = SOME n
  | getFunBONext (FunBindOST (_, _, _, _, _, _, n))   = SOME n
  | getFunBONext (FunBindODots pl)                    = getPartListNext pl

and getLabSigExpNext (LabSigExp (_, _, _, _, n))     = SOME n
  | getLabSigExpNext (LabSigExpDots pl)              = getPartListNext pl

and getSigExpNext (SigExpBasic (_, _, _, n))         = SOME n
  | getSigExpNext (SigExpId (_, _, n))               = SOME n
  | getSigExpNext (SigExpRea (_, _, _, _, n))        = SOME n
  | getSigExpNext (SigExpDots pl)                    = getPartListNext pl

and getLabStrExpNext (LabStrExp (_, _, _, _, n))     = SOME n
  | getLabStrExpNext (LabStrExpDots pl)              = getPartListNext pl

and getStrExpNext (StrExpBasic (_, _, _, n))         = SOME n
  | getStrExpNext (StrExpId (_, _, n))               = SOME n
  | getStrExpNext (StrExpOp (_, _, _, _, n))         = SOME n
  | getStrExpNext (StrExpTr (_, _, _, _, n))         = SOME n
  | getStrExpNext (StrExpFExp (_, _, _, _, n))       = SOME n
  | getStrExpNext (StrExpFDec (_, _, _, _, n))       = SOME n
  | getStrExpNext (StrExpLocal (_, _, _, _, n))      = SOME n
  | getStrExpNext (StrExpDots pl)                    = getPartListNext pl

and getSpecOneNext (SpecValue (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecType (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecEqtype (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecException (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecTdr (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecDat (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecStr (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecInc (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecIsi (_, _, _, n))            = SOME n
  | getSpecOneNext (SpecRep (_, _, _, _, n))         = SOME n
  | getSpecOneNext (SpecSha (_, _, _, _, n))         = SOME n
  | getSpecOneNext (SpecSsi (_, _, _, _, n))         = SOME n
  | getSpecOneNext (SpecOneDots pl)                  = getPartListNext pl

and getSpecNext (Spec (_, n))                        = SOME n
  | getSpecNext (SpecDots pl)                        = getPartListNext pl

and getConDescOneNext (ConDescOneId (_, n))          = SOME n
  | getConDescOneNext (ConDescOneOf (_, _, _, _, n)) = SOME n
  | getConDescOneNext (ConDescOneNoOf (_, n))        = SOME n
  | getConDescOneNext (ConDescOneDots pl)            = getPartListNext pl

and getDatDescNext (DatDesc (_, _, n))               = SOME n
  | getDatDescNext (DatDescDots pl)                  = getPartListNext pl

and getDatDescOneNext (DatDescOne (_, _, _, _, n))   = SOME n
  | getDatDescOneNext (DatDescOneDots pl)            = getPartListNext pl

and getStrDescNext (StrDesc (_, _, n))               = SOME n
  | getStrDescNext (StrDescDots pl)                  = getPartListNext pl

and getStrDescOneNext (StrDescOne (_, _, _, _, n))   = SOME n
  | getStrDescOneNext (StrDescOneDots pl)            = getPartListNext pl

and getValDescNext (ValDesc (_, _, n))               = SOME n
  | getValDescNext (ValDescDots pl)                  = getPartListNext pl

and getValDescOneNext (ValDescOne (_, _, _, _, n))   = SOME n
  | getValDescOneNext (ValDescOneDots pl)            = getPartListNext pl

and getTypDescNext (TypDesc (_, _, n))               = SOME n
  | getTypDescNext (TypDescDots pl)                  = getPartListNext pl

and getTypDescOneNext (TypDescOne (_, _, n))         = SOME n
  | getTypDescOneNext (TypDescOneDots pl)            = getPartListNext pl

and getExcDescNext (ExcDesc (_, _, n))               = SOME n
  | getExcDescNext (ExcDescDots pl)                  = getPartListNext pl

and getExcDescOneNext (ExcDescOne (_, _, n))         = SOME n
  | getExcDescOneNext (ExcDescOf (_, _, _, _, n))    = SOME n
  | getExcDescOneNext (ExcDescOneDots pl)            = getPartListNext pl

and getTdrDescNext (TdrDesc (_, _, n))               = SOME n
  | getTdrDescNext (TdrDescDots pl)                  = getPartListNext pl

and getTdrDescOneNext (TdrDescOne (_, _, _, _, n))   = SOME n
  | getTdrDescOneNext (TdrDescOneDots pl)            = getPartListNext pl

and getATopDecNext (ATopDecStr s)                    = getStrDecNext s
  | getATopDecNext (ATopDecSig s)                    = getSigDecNext s
  (*| getATopDecNext (ATopDecFun f)                  = getFunDecNext f*)
  | getATopDecNext (ATopDecDots pl)                  = getPartListNext pl

and getSmlTesNext (SmlTesDec   (_, _, n))            = SOME n
  | getSmlTesNext (SmlTesSpec  (_, _, n))            = SOME n
  | getSmlTesNext (SmlTesUse   (_, _, n))            = SOME n
  | getSmlTesNext (SmlTesSBas  (_, _, n))            = SOME n
  | getSmlTesNext (SmlTesCBas  (_, _, n))            = SOME n
  | getSmlTesNext (SmlTesQuote (_, _, n))            = SOME n
  | getSmlTesNext (SmlTesType  (_, _, n))            = SOME n
  | getSmlTesNext (SmlTesDots pl)                    = getPartListNext pl

and getTopDecOneNext (TopDecOneTes (_, n))           = SOME n
  | getTopDecOneNext (TopDecOneDec (_, n))           = SOME n
  | getTopDecOneNext (TopDecOneDots pl)              = getPartListNext pl

and getTopDecOneListNext []                          = NONE
  | getTopDecOneListNext [x]                         = getTopDecOneNext x
  | getTopDecOneListNext (_ :: xs)                   = getTopDecOneListNext xs

and getTopDecNext (TopDec xs)                        = getTopDecOneListNext xs
  | getTopDecNext (TopDecDots pl)                    = getPartListNext pl

and getProgOneNext (ProgOneDec td)                   = getTopDecNext td
  | getProgOneNext (ProgOneExp (_, _, _, _, n))      = SOME n
  | getProgOneNext (ProgOneParse (_, _, _, n))       = SOME n
  | getProgOneNext (ProgOneFile (_, n))              = SOME n
  | getProgOneNext (ProgOneDots pl)                  = getPartListNext pl

and getProgOneListNext []                            = NONE
  | getProgOneListNext [x]                           = getProgOneNext x
  | getProgOneListNext (_ :: xs)                     = getProgOneListNext xs

and getProgNext (Prog xs)                            = getProgOneListNext xs
  | getProgNext (ProgDots pl)                        = getPartListNext pl


(* First ones *)


fun getFirstList xs f =
    foldr (fn (x, y) =>
	      case (f x, y) of
		  (NONE,   NONE)   => NONE
		| (SOME u, NONE)   => SOME u
		| (NONE,   SOME u) => SOME u
		| (SOME u, SOME v) => SOME (L.min u v))
	  NONE
	  xs

fun getPartListFirst []                               = NONE
  | getPartListFirst (x :: _)                         = getPartFirst x

and getPartFirst (PartExp   e)                        = getExpFirst       e
  | getPartFirst (PartDec   d)                        = getDecFirst       d
  | getPartFirst (PartType  t)                        = getTypeFirst      t
  | getPartFirst (PartSeq   s)                        = getTypeRowFirst   s
  | getPartFirst (PartPat   p)                        = getPatFirst       p
  | getPartFirst (PartIdTy  i)                        = getIdentTyFirst   i
  | getPartFirst (PartTyCon t)                        = getLongTyConFirst t
  | getPartFirst (PartSpec  s)                        = getSpecOneFirst   s
  | getPartFirst (PartSige  s)                        = getSigExpFirst    s
  | getPartFirst (PartStre  s)                        = getStrExpFirst    s
  (*| getPartFirst (PartFund  f)                        = getFunDecFirst    f*)
  | getPartFirst (PartSigd  s)                        = getSigDecFirst    s
  | getPartFirst (PartStrd  s)                        = getStrDecOneFirst s
  | getPartFirst (PartLgid  i)                        = getLongIdFirst    i
  | getPartFirst (PartLgsid i)                        = getLongStrIdFirst i
  | getPartFirst (PartSigid i)                        = getSigIdFirst     i
  | getPartFirst (PartTes   t)                        = getSmlTesFirst    t
  | getPartFirst (PartClass c)                        = getClassFirst     c

and getSconFirst (SconInt    (_, _, _, l, _))         = SOME l
  | getSconFirst (SconWord   (_, _, _, l, _))         = SOME l
  | getSconFirst (SconReal   (_, _, _, l, _))         = SOME l
  | getSconFirst (SconString (_, _, _, l, _))         = SOME l
  | getSconFirst (SconChar   (_, _, _, l, _))         = SOME l
  | getSconFirst SconDots                             = NONE

and getPconFirst (PconBool (_, _, _, l, _))           = SOME l
  | getPconFirst (PconNil  (_, _, _, l, _))           = SOME l
  | getPconFirst (PconRef  (_, _, _, l, _))           = SOME l
  | getPconFirst PconDots                             = NONE

and getIdentFirst (Ident (_, _, _, l, _))             = SOME l
  | getIdentFirst (IdentPcon pc)                      = getPconFirst pc
  | getIdentFirst IdentDots                           = NONE

and getClassFirst (Class (_, _, _, l, _))             = SOME l
  | getClassFirst ClassDots                           = NONE

and getLongIdFirst (LongIdQual (_, _, _, l, _))       = SOME l
  | getLongIdFirst (LongIdId id)                      = getIdentFirst id
  | getLongIdFirst (LongIdDots pl)                    = getPartListFirst pl

and getIdentTyFirst (IdentTyId id)                    = getIdentFirst id
  | getIdentTyFirst (IdentTyTy (_, _, _, l, _))       = SOME l
  | getIdentTyFirst (IdentTyDots pl)                  = getPartListFirst pl

and getStrIdFirst (StrId (_, _, _, l, _))             = SOME l
  | getStrIdFirst StrIdDots                           = NONE

and getLongStrIdFirst (LongStrIdQual (_, _, _, l, _)) = SOME l
  | getLongStrIdFirst (LongStrIdId id)                = getStrIdFirst id
  | getLongStrIdFirst (LongStrIdDots pl)              = getPartListFirst pl

and getSigIdFirst (SigId (_, _, _, l, _))             = SOME l
  | getSigIdFirst SigIdDots                           = NONE

and getAtExpFirst (AtExpId i)                         = getLongIdFirst i
  | getAtExpFirst (AtExpScon s)                       = getSconFirst s
  | getAtExpFirst (AtExpTuple (_, _, l, _))           = SOME l
  | getAtExpFirst (AtExpRecord (_, _, _, l, _))       = SOME l
  | getAtExpFirst (AtExpSlRec (_, _, l, _))           = SOME l
  | getAtExpFirst (AtExpLet (_, _, _, l, _))          = SOME l
  | getAtExpFirst (AtExpDLet (_, _, _, l, _))         = SOME l
  | getAtExpFirst (AtExpParen (_, _, _, l, _))        = SOME l
  | getAtExpFirst (AtExpList (_, _, l, _))            = SOME l
  | getAtExpFirst (AtExpProj (_, _, _, l, _))         = SOME l
  | getAtExpFirst (AtExpSeq (_, _, l, _))             = SOME l
  | getAtExpFirst (AtExpQuote (_, _, l, _))           = SOME l
  | getAtExpFirst (AtExpDots pl)                      = getPartListFirst pl

and getExpFirst (ExpAtExp a)                          = getAtExpFirst a
  | getExpFirst (ExpFn (_, _, l, _))                  = SOME l
  | getExpFirst (ExpApp (_, _, _, _, _, l, _))        = SOME l
  | getExpFirst (ExpCase (_, _, _, _, l, _))          = SOME l
  | getExpFirst (ExpConsList (_, _, _, _, l, _))      = SOME l
  | getExpFirst (ExpOp (_, _, _, _, _, l, _))         = SOME l
  | getExpFirst (ExpOr (_, _, _, l, _))               = SOME l
  | getExpFirst (ExpAnd (_, _, _, l, _))              = SOME l
  | getExpFirst (ExpTyped (_, _, _, l, _))            = SOME l
  | getExpFirst (ExpIte (_, _, _, _, l, _))           = SOME l
  | getExpFirst (ExpWhile (_, _, _, _, l, _))         = SOME l
  | getExpFirst (ExpRaise (_, _, l, _))               = SOME l
  | getExpFirst (ExpHandle (_, _, _, l, _))           = SOME l
  | getExpFirst (ExpDots pl)                          = getPartListFirst pl

and getAtPatFirst (AtPatWild _)                       = NONE
  | getAtPatFirst (AtPatId id)                        = getLongIdFirst id
  | getAtPatFirst (AtPatScon sc)                      = getSconFirst sc
  | getAtPatFirst (AtPatTuple (_, _, l, _))           = SOME l
  | getAtPatFirst (AtPatRecord (_, _, _, l, _))       = SOME l
  | getAtPatFirst (AtPatParen (_, _, _, l, _))        = SOME l
  | getAtPatFirst (AtPatList (_, _, l, _))            = SOME l
  | getAtPatFirst (AtPatOr (_, _, l, _))              = SOME l
  | getAtPatFirst (AtPatDots pl)                      = getPartListFirst pl

and getPatFirst (PatAtPat a)                          = getAtPatFirst a
  | getPatFirst (PatApp (_, _, _, _, l, _))           = SOME l
  | getPatFirst (PatConsList (_, _, _, _, l, _))      = SOME l
  | getPatFirst (PatOp (_, _, _, _, _, l, _))         = SOME l
  | getPatFirst (PatTyped (_, _, _, l, _))            = SOME l
  | getPatFirst (PatAs (_, _, _, l, _))               = SOME l
  | getPatFirst (PatDots pl)                          = getPartListFirst pl

and getStrBOFirst (StrBindOneOp (_, _, _, _, l, _))   = SOME l
  | getStrBOFirst (StrBindOneTr (_, _, _, _, l, _))   = SOME l
  | getStrBOFirst (StrBindOne (_, _ , _, l, _))       = SOME l
  | getStrBOFirst (StrBindOneDots pl)                 = getPartListFirst pl

and getStrBOLFirst []                                 = NONE
  | getStrBOLFirst (x :: _)                           = getStrBOFirst x

and getStrBindFirst (StrBind (s, _, _))               = getStrBOLFirst s
  | getStrBindFirst (StrBindDots pl)                  = getPartListFirst pl

and getTypeRowFirst (TypeRowOne (_, _, l, _))         = SOME l
  | getTypeRowFirst (TypeRowEm (_, l, _))             = SOME l
  | getTypeRowFirst (TypeRowSeq (_, _, l, _))         = SOME l
  | getTypeRowFirst (TypeRowDots pl)                  = getPartListFirst pl

and getTyConFirst (TyCon (_, _, _, l, _))             = SOME l
  | getTyConFirst TyConDots                           = NONE

and getLongTyConFirst (LongTyConQual (_, _, _, l, _)) = SOME l
  | getLongTyConFirst (LongTyConId tc)                = getTyConFirst tc
  | getLongTyConFirst (LongTyConDots pl)              = getPartListFirst pl

and getTypeFirst (TypeOneVar tv)                      = getTypeVarFirst tv
  | getTypeFirst (TypeArrow (_, _, _, l, _))          = SOME l
  | getTypeFirst (TypeTuple (_, _, l, n))             = SOME l
  | getTypeFirst (TypeRecord (_, _, _, l, _))         = SOME l
  | getTypeFirst (TypeSlRec (_, _, l, _))             = SOME l
  | getTypeFirst (TypeTyCon (_, _, _, l, _))          = SOME l
  | getTypeFirst (TypeParen (_, _, _, l, _))          = SOME l
  | getTypeFirst (TypeDots pl)                        = getPartListFirst pl

and getTypeVarFirst (TypeVar (_, _, _, l, _))         = SOME l
  | getTypeVarFirst TypeVarDots                       = NONE

and getTypeVarListFirst []                            = NONE
  | getTypeVarListFirst (x :: _)                      = getTypeVarFirst x

and getTypeVarSeqFirst (TypeVarSeqOne (_, _, l, _))       = SOME l
  | getTypeVarSeqFirst (TypeVarSeqEm (_, l, _))           = SOME l
  | getTypeVarSeqFirst (TypeVarSeqSeq (_, _, l, _))       = SOME l
  | getTypeVarSeqFirst (TypeVarSeqDots tvl)               = getTypeVarListFirst tvl

and getExBindFirst (ExBind (_, l, _))                 = SOME l
  | getExBindFirst (ExBindOf (_, _, _, l, _))         = SOME l
  | getExBindFirst (ExBindEq (_, _, _, l, _))         = SOME l
  | getExBindFirst (ExBindNo (i, _))                  = getIdentFirst i
  | getExBindFirst (ExBindDots pl)                    = getPartListFirst pl

and getExBindListFirst []                             = NONE
  | getExBindListFirst (x :: _)                       = getExBindFirst x

and getExBindSeqFirst (ExBindSeq (e, _, _))           = getExBindListFirst e
  | getExBindSeqFirst (ExBindSeqDots pl)              = getPartListFirst pl

and getTypBindFirst (TypBind (_, _, _, l, _))         = SOME l
  | getTypBindFirst (TypBindDots pl)                  = getPartListFirst pl

and getTypBindListFirst []                            = NONE
  | getTypBindListFirst (x :: _)                      = getTypBindFirst x

and getTypBindSeqFirst (TypBindSeq (t, _, _))         = getTypBindListFirst t
  | getTypBindSeqFirst (TypBindSeqDots pl)            = getPartListFirst pl

and getDatBindFirst (DatBind ( _, _, _, l, _))        = SOME l
  | getDatBindFirst (DatBindDots pl)                  = getPartListFirst pl

and getDatBindListFirst []                            = NONE
  | getDatBindListFirst (x :: _)                      = getDatBindFirst x

and getDatBindSeqFirst (DatBindSeq (d, _, _))         = getDatBindListFirst d
  | getDatBindSeqFirst (DatBindSeqDots pl)            = getPartListFirst pl

and getDecFirst (DecVal (t, _, _, _))                 = getTypeVarSeqFirst t
  | getDecFirst (DecFVal (t, _, _, _))                = getTypeVarSeqFirst t
  | getDecFirst (DecDatType (d, _, _))                = getDatBindSeqFirst d
  | getDecFirst (DecDatWith (_, _, _, l, _))          = SOME l
  | getDecFirst (DecDatRep (_, _, _, l, _))           = SOME l
  | getDecFirst (DecType (t, _, n))                   = getTypBindSeqFirst t
  | getDecFirst (DecEx (e, _, _))                     = getExBindSeqFirst e
  | getDecFirst (DecOpen (_, _, l, _))                = SOME l
  | getDecFirst (DecLocal (_, _, _, l, _))            = SOME l
  | getDecFirst (DecAbsType (_, _, _, l, _))          = SOME l
  | getDecFirst (DecAbsWith (_, _, _, _, l, _))       = SOME l
  | getDecFirst (DecInfix  (_, _, _, l, _))           = SOME l
  | getDecFirst (DecInfixr (_, _, _, l, _))           = SOME l
  | getDecFirst (DecNonfix (_, _, l, _))              = SOME l
  | getDecFirst (DecOverload (_, _, _, _, _, l, _))   = SOME l
  | getDecFirst (DecClass (_, _, _, l, _))            = SOME l
  | getDecFirst (DecDots pl)                          = getPartListFirst pl

and getDecListFirst []                                = NONE
  | getDecListFirst (x :: _)                          = getDecFirst x

and getDecsFirst (Decs (d, _))                        = getDecListFirst d
  | getDecsFirst (DecsDots pl)                        = getPartListFirst pl

and getStrDecOneListFirst []                          = NONE
  | getStrDecOneListFirst (x :: _)                    = getStrDecOneFirst x

and getStrDecOneFirst (StrDecOneDec d)                = getDecsFirst d
  | getStrDecOneFirst (StrDecOneStr (s, _, n))        = getStrBindFirst s
  | getStrDecOneFirst (StrDecOneLoc (_, _, _, l, _))  = SOME l
  | getStrDecOneFirst (StrDecOneFun (_, _, l, _))     = SOME l
  | getStrDecOneFirst (StrDecOneDots pl)              = getPartListFirst pl

and getSigBindOneFirst (SigBindOne (_, _, _, l, _))   = SOME l
  | getSigBindOneFirst (SigBindOneDots pl)            = getPartListFirst pl

and getSigBindOneListFirst []                         = NONE
  | getSigBindOneListFirst (x :: _)                   = getSigBindOneFirst x

and getSigBindFirst (SigBind (s, _, _))               = getSigBindOneListFirst s
  | getSigBindFirst (SigBindDots pl)                  = getPartListFirst pl

and getSigDecFirst (SigDec (s, _, _))                 = getSigBindFirst s
  | getSigDecFirst (SigDecDots pl)                    = getPartListFirst pl

and getStrDecFirst (StrDec (s, _, _))                 = getStrDecOneListFirst s
  | getStrDecFirst (StrDecDots pl)                    = getPartListFirst pl

and getValDescOneFirst (ValDescOne (_, _, _, l, _))   = SOME l
  | getValDescOneFirst (ValDescOneDots pl)            = getPartListFirst pl

and getAFileFirst (AFile (_, _, l, _))                = SOME l
  | getAFileFirst AFileDots                           = NONE

and getABoolFirst (ABool (_, _, l, _))                = SOME l
  | getABoolFirst ABoolDots                           = NONE

and getLabStrExpFirst (LabStrExp (_, _, _, l, _))     = SOME l
  | getLabStrExpFirst (LabStrExpDots pl)              = getPartListFirst pl

and getStrExpFirst (StrExpBasic (_, _, l, _))         = SOME l
  | getStrExpFirst (StrExpId (_, l, _))               = SOME l
  | getStrExpFirst (StrExpOp (_, _, _, l, _))         = SOME l
  | getStrExpFirst (StrExpTr (_, _, _, l, _))         = SOME l
  | getStrExpFirst (StrExpFExp (_, _, _, l, _))       = SOME l
  | getStrExpFirst (StrExpFDec (_, _, _, l, _))       = SOME l
  | getStrExpFirst (StrExpLocal (_, _, _, l, _))      = SOME l
  | getStrExpFirst (StrExpDots pl)                    = getPartListFirst pl

and getLabSigExpFirst (LabSigExp (_, _, _, l, _))     = SOME l
  | getLabSigExpFirst (LabSigExpDots pl)              = getPartListFirst pl

and getSigExpFirst (SigExpBasic (_, _, l, _))         = SOME l
  | getSigExpFirst (SigExpId (_, l, _))               = SOME l
  | getSigExpFirst (SigExpRea (_, _, _, l, _))        = SOME l
  | getSigExpFirst (SigExpDots pl)                    = getPartListFirst pl

and getSpecOneFirst (SpecValue (_, _, l, _))          = SOME l
  | getSpecOneFirst (SpecType (_, _, l, _))           = SOME l
  | getSpecOneFirst (SpecEqtype (_, _, l, _))         = SOME l
  | getSpecOneFirst (SpecException (_, _, l, _))      = SOME l
  | getSpecOneFirst (SpecTdr (_, _, l, _))            = SOME l
  | getSpecOneFirst (SpecDat (_, _, l, _))            = SOME l
  | getSpecOneFirst (SpecStr (_, _, l, _))            = SOME l
  | getSpecOneFirst (SpecInc (_, _, l, _))            = SOME l
  | getSpecOneFirst (SpecIsi (_, _, l, _))            = SOME l
  | getSpecOneFirst (SpecRep (_, _, _, l, _))         = SOME l
  | getSpecOneFirst (SpecSha (_, _, _, l, _))         = SOME l
  | getSpecOneFirst (SpecSsi (_, _, _, l, _))         = SOME l
  | getSpecOneFirst (SpecOneDots pl)                  = getPartListFirst pl

and getSpecFirst (Spec (specs, _))                    = getFirstList specs getSpecOneFirst
  | getSpecFirst (SpecDots pl)                        = getPartListFirst pl

and getATopDecFirst (ATopDecStr s)                    = getStrDecFirst s
  | getATopDecFirst (ATopDecSig s)                    = getSigDecFirst s
  (*| getATopDecFirst (ATopDecFun f)                    = getFunDecFirst f*)
  | getATopDecFirst (ATopDecDots pl)                  = getPartListFirst pl

and getSmlTesFirst (SmlTesDec  (t, _, _))             = getATopDecFirst t
  | getSmlTesFirst (SmlTesSpec (s, _, _))             = getSpecFirst    s
  | getSmlTesFirst (SmlTesUse  (f, _, _))             = getAFileFirst   f
  | getSmlTesFirst (SmlTesSBas (f, _, _))             = getAFileFirst   f
  | getSmlTesFirst (SmlTesCBas (_, l, _))             = SOME l
  | getSmlTesFirst (SmlTesQuote (b, _, _))            = getABoolFirst b
  | getSmlTesFirst (SmlTesType (_, _, n))             = SOME n
  | getSmlTesFirst (SmlTesDots pl)                    = getPartListFirst pl

and getTopDecOneFirst (TopDecOneTes (x, _))           = getSmlTesFirst x
  | getTopDecOneFirst (TopDecOneDec (x, _))           = getATopDecFirst x
  | getTopDecOneFirst (TopDecOneDots pl)              = getPartListFirst pl

and getTopDecOneListFirst xs                          = getFirstList xs getTopDecOneFirst

and getTopDecFirst (TopDec t)                         = getTopDecOneListFirst t
  | getTopDecFirst (TopDecDots pl)                    = getPartListFirst pl

and getProgOneFirst (ProgOneDec t)                    = getTopDecFirst t
  | getProgOneFirst (ProgOneExp (_, _, _, l, _))      = SOME l
  | getProgOneFirst (ProgOneParse (_, _, l, _))       = SOME l
  | getProgOneFirst (ProgOneFile (f, _))              = getAFileFirst f
  | getProgOneFirst (ProgOneDots pl)                  = getPartListFirst pl

and getProgOneListFirst xs                            = getFirstList xs getProgOneFirst

and getProgFirst (Prog xs)                            = getProgOneListFirst xs
  | getProgFirst (ProgDots pl)                        = getPartListFirst pl


(* expressions within a rec value binding must be functions *)


fun isExpFnScon (SconInt    (_, _, _, l, _)) = L.singleton l
  | isExpFnScon (SconWord   (_, _, _, l, _)) = L.singleton l
  | isExpFnScon (SconReal   (_, _, _, l, _)) = L.singleton l
  | isExpFnScon (SconString (_, _, _, l, _)) = L.singleton l
  | isExpFnScon (SconChar   (_, _, _, l, _)) = L.singleton l
  | isExpFnScon SconDots                     = L.empty

fun isExpFnPcon (PconBool (_, _, _, l, _)) = L.singleton l
  | isExpFnPcon (PconNil  (_, _, _, l, _)) = L.singleton l
  | isExpFnPcon (PconRef  (_, _, _, l, _)) = L.singleton l
  | isExpFnPcon PconDots                   = L.empty

fun isExpFnId (Ident (_, _, _, l, _)) = L.singleton l
  | isExpFnId (IdentPcon pc)          = isExpFnPcon pc
  | isExpFnId IdentDots               = L.empty

fun isExpFnLongId (LongIdQual (_, _, _, l, _)) = L.singleton l
  | isExpFnLongId (LongIdId id)                = isExpFnId id
  | isExpFnLongId (LongIdDots _)               = L.empty

fun isExpFnLabExp (LabExp (exp, _, _, l, _)) =
    let
	val labs = isExpFnExp exp
    in if L.isEmpty labs
       then labs
       else L.cons l labs
    end
  | isExpFnLabExp (LabExpDots _)          = L.empty

and isExpFnExp (ExpAtExp exp)                   = isExpFnAtExp exp
  | isExpFnExp (ExpFn _)                        = L.empty
  | isExpFnExp (ExpApp (_, _, _, _, _, l, _))   = L.singleton l
  | isExpFnExp (ExpCase (_, _, _, _, l, _))     = L.singleton l
  | isExpFnExp (ExpConsList (_, _, _, _, l, _)) = L.singleton l
  | isExpFnExp (ExpOp (_, _, _, _, _, l, _))    = L.singleton l
  | isExpFnExp (ExpOr (_, _, _, l, _))          = L.singleton l
  | isExpFnExp (ExpAnd (_, _, _, l, _))         = L.singleton l
  | isExpFnExp (ExpTyped (_, _, _, l, _))       = L.singleton l
  | isExpFnExp (ExpIte (_, _, _, _, l, _))      = L.singleton l
  | isExpFnExp (ExpWhile (_, _, _, _, l, _))    = L.singleton l
  | isExpFnExp (ExpRaise (_, _, l, _))          = L.singleton l
  | isExpFnExp (ExpHandle (_, _, _, l, _))      = L.singleton l
  | isExpFnExp (ExpDots _)                      = L.empty

and isExpFnAtExp (AtExpId id)                  = isExpFnLongId id
  | isExpFnAtExp (AtExpScon sc)                = isExpFnScon sc
  | isExpFnAtExp (AtExpTuple (_, _, l, _))     = L.singleton l
  | isExpFnAtExp (AtExpRecord (_, _, _, l, _)) = L.singleton l
  | isExpFnAtExp (AtExpSlRec (_, _, l, _))     = L.singleton l
  | isExpFnAtExp (AtExpLet (_, _, _, l, _))    = L.singleton l
  | isExpFnAtExp (AtExpDLet (_, _, _, l, _))   = L.singleton l
  | isExpFnAtExp (AtExpParen (e, _, _, l, _))  =
    let
	val labs = isExpFnLabExp e
    in if L.isEmpty labs
       then labs
       else L.cons l labs
    end
  | isExpFnAtExp (AtExpList (_, _, l, _))      = L.singleton l
  | isExpFnAtExp (AtExpProj (_, _, _, l, _))   = L.singleton l
  | isExpFnAtExp (AtExpSeq (_, _, l, _))       = L.singleton l
  | isExpFnAtExp (AtExpQuote (_, _, l, _))     = L.singleton l
  | isExpFnAtExp (AtExpDots _)                 = L.empty

(* returns labels *)
fun isExpFnValBindCore (ValBindCore (_, exp, _, l, _)) =
    let
	val labs = isExpFnLabExp exp
    in if L.isEmpty labs
       then labs
       else L.cons l labs
    end
  | isExpFnValBindCore (ValBindCoreDots _)             = L.empty

(* returns a list of labels *)
fun isExpFnValBindSeq (ValBindSeq (xs, _, _)) =
    List.mapPartial
	(fn x =>
	    let
		val labs = isExpFnValBindCore x
	    in if L.isEmpty labs
	       then NONE
	       else SOME labs
	    end) xs
  | isExpFnValBindSeq (ValBindSeqDots _)      = []

(*(* returns a list of labels *)
fun isExpFnValBind (ValBindRec (vbs, _, l, _)) = isExpFnValBindSeq vbs
  | isExpFnValBind (ValBind     _)             = []
  | isExpFnValBind (ValBindDots _)             = []*)


(* within a pattern, there cannot be real constants *)


fun isPatScon (SconInt    _)             = NONE
  | isPatScon (SconWord   _)             = NONE
  | isPatScon (SconReal (_, _, _, l, _)) = SOME l
  | isPatScon (SconString _)             = NONE
  | isPatScon (SconChar   _)             = NONE
  | isPatScon SconDots                   = NONE


(* extract the files used by another one *)

fun extractFilesAFile (AFile (f, r, l, n)) bbas = [(f, r, bbas)]
  | extractFilesAFile _ _ = []

fun extractFilesSmlTes (SmlTesUse  (af, _, _)) = extractFilesAFile af false
  | extractFilesSmlTes (SmlTesSBas (af, _, _)) = extractFilesAFile af true
  | extractFilesSmlTes _  = []

fun extractFilesTopDecOne (TopDecOneTes (x, _)) = extractFilesSmlTes x
  | extractFilesTopDecOne _ = []

fun extractFilesTopDec (TopDec xs) = foldr (fn (x, y) => (extractFilesTopDecOne x) @ y) [] xs
  | extractFilesTopDec _ = []

fun extractFilesProgOne (ProgOneDec x)        = extractFilesTopDec x
  | extractFilesProgOne (ProgOneFile (af, _)) = extractFilesAFile af false
  | extractFilesProgOne _  = []

fun extractFilesProg (Prog xs) = foldr (fn (x, y) => (extractFilesProgOne x) @ y) [] xs
  | extractFilesProg _ = []


(**)


fun combineProg (Prog xs) (Prog ys) = Prog (xs @ ys)
  | combineProg _ _ = raise EH.DeadBranch ""


(**)


fun isClearBasisSmlTes (SmlTesCBas _) = true
  | isClearBasisSmlTes _  = false

fun isClearBasisTopDecOne (TopDecOneTes (x, _)) = isClearBasisSmlTes x
  | isClearBasisTopDecOne _ = false

fun isClearBasisTopDec (TopDec xs) =
    foldr (fn (x, y) => (isClearBasisTopDecOne x) orelse y) false xs
  | isClearBasisTopDec _ = false

fun isClearBasisProgOne (ProgOneDec x)  = isClearBasisTopDec x
  | isClearBasisProgOne (ProgOneFile _) = false
  | isClearBasisProgOne _  = false

fun isClearBasisProg (Prog xs) =
    foldr (fn (x, y) => (isClearBasisProgOne x) orelse y) false xs
  | isClearBasisProg _ = false


(* Extract the identifier from an identifier row.
 * This is used by the parser to record the infix operators.*)

fun getNameIdent (Ident (st, _, _, _, _)) = SOME st
  | getNameIdent (IdentPcon _) = NONE
  | getNameIdent IdentDots = NONE

fun getNamesIdentSeq (IdentSeq (idents, _)) = List.mapPartial getNameIdent idents
  | getNamesIdentSeq (IdentSeqDots _) = []

end
