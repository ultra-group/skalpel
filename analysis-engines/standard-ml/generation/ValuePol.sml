(* Copyright 2010 Heriot-Watt University
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
 *  o Date:        16 July 2010
 *  o File name:   ValuePol.sml
 *
 *)

(** Deals with the value polymorphism restriction, constrained opaquely by refstruct{VALUEPOL}. *)
structure ValuePol :> VALUEPOL = struct

(* abbreviate the names of the structures we use *)
structure I  = Id
structure A  = AstSML
structure X  = Expans
structure L  = Label
structure EH = ErrorHandler

(** Returns the top label of any of the scon constructors of the ast. *)
fun toplabScon (A.SconInt    (_, _, _, l, _)) = [l]
  | toplabScon (A.SconWord   (_, _, _, l, _)) = [l]
  | toplabScon (A.SconReal   (_, _, _, l, _)) = [l]
  | toplabScon (A.SconString (_, _, _, l, _)) = [l]
  | toplabScon (A.SconChar   (_, _, _, l, _)) = [l]
  | toplabScon SconDots                     = []

(** Returns the top label of any of the pcon constructors in the ast. *)
and toplabPcon (A.PconBool (_, _, _, l, _)) = [l]
  | toplabPcon (A.PconNil  (_, _, _, l, _)) = [l]
  | toplabPcon (A.PconRef  (_, _, _, l, _)) = [l]
  | toplabPcon A.PconDots                   = []

(** Returns the top label of any of the ident constructors in the ast. *)
and toplabIdent (A.Ident (_, _, _, l, _)) = [l]
  | toplabIdent (A.IdentPcon pc)          = toplabPcon pc
  | toplabIdent A.IdentDots               = []

(** Returns the top label of any of the longid constructors in the ast. *)
and toplabLongId (A.LongIdQual (_, _, _, l, _)) = [l]
  | toplabLongId (A.LongIdId id)                = toplabIdent id
  | toplabLongId (A.LongIdDots _)               = []

(** Returns the top label of any of the atomic expressions (atexp) constructors in the ast. *)
fun toplabAtExp (A.AtExpId id)                    = toplabLongId id
  | toplabAtExp (A.AtExpScon sc)                  = toplabScon sc
  | toplabAtExp (A.AtExpTuple (_, _, l, _))       = [l]
  | toplabAtExp (A.AtExpRecord (_, _, _, l,_))    = [l]
  | toplabAtExp (A.AtExpSlRec (_, _, l, _))       = [l]
  | toplabAtExp (A.AtExpLet (_, _, _, l, _))      = [l]
  | toplabAtExp (A.AtExpDLet (_, _, _, l, _))     = [l]
  | toplabAtExp (A.AtExpParen (_, _, _, l, _))    = [l]
  | toplabAtExp (A.AtExpList (_, _, l, _))        = [l]
  | toplabAtExp (A.AtExpProj (_, _, _, l, _))     = [l]
  | toplabAtExp (A.AtExpSeq (_, _, l, _))         = [l]
  | toplabAtExp (A.AtExpQuote (_, _, l, _))       = [l]
  | toplabAtExp (A.AtExpDots _)                   = []

(** Returns the top label of any of the expression (exp) constructors in the ast. *)
and toplabExp (A.ExpAtExp ae)                    = toplabAtExp ae
  | toplabExp (A.ExpFn (_, _, l, _))             = [l]
  | toplabExp (A.ExpApp (_, _, _, _, _, l, _))   = [l]
  | toplabExp (A.ExpCase (_, _, _, _, l, _))     = [l]
  | toplabExp (A.ExpConsList (_, _, _, _, l, _)) = [l]
  | toplabExp (A.ExpOp (_, _, _, _, _, l, _))    = [l]
  | toplabExp (A.ExpOr (_, _, _, l, _))          = [l]
  | toplabExp (A.ExpAnd (_, _, _, l, _))         = [l]
  | toplabExp (A.ExpTyped (_, _, _, l, _))       = [l]
  | toplabExp (A.ExpIte (_, _, _, _, l, _))      = [l]
  | toplabExp (A.ExpWhile (_, _, _, _, l, _))    = [l]
  | toplabExp (A.ExpRaise (_, _, l, _))          = [l]
  | toplabExp (A.ExpHandle (_, _, _, l, _))      = [l]
  | toplabExp (A.ExpDots _)                      = []

(** Returns the top label of any of the labexp constructors in the ast. *)
and toplabLabExp (A.LabExp (e, _, _, l, _)) = l :: (toplabExp e)
  | toplabLabExp (A.LabExpDots _)           = []

(** Returns expansiveness of an atomic expression. *)
fun nonexpAtExp (A.AtExpId id)                    = X.Nonexp
  | nonexpAtExp (A.AtExpScon _)                   = X.Nonexp
  | nonexpAtExp (A.AtExpTuple (lel, _, l, _))     = X.addnonexp (nonexpLabExpList lel) l
  | nonexpAtExp (A.AtExpRecord (erl, _, _, l, _)) = X.addnonexp (nonexpExpFieldList erl) l
  | nonexpAtExp (A.AtExpSlRec (erl, _, l, _))     = X.addnonexp (nonexpExpFieldList erl) l
  | nonexpAtExp (A.AtExpLet (_, _, _, l, _))      = X.genOneExpans l
  | nonexpAtExp (A.AtExpDLet (_, _, _, l, _))     = X.genOneExpans l
  | nonexpAtExp (A.AtExpParen (le, _, _, l, _))   = X.addnonexp (nonexpLabExp le) l
  | nonexpAtExp (A.AtExpList (lel, _, l, _))      = X.addnonexp (nonexpLabExpList lel) l
  | nonexpAtExp (A.AtExpProj (_, _, _, l, _))     = X.genOneExpans l (* TODO: check unresolved flexible record *)
  | nonexpAtExp (A.AtExpSeq (seq, _, l, _))       = X.addnonexp (nonexpSeqExp seq) l
  | nonexpAtExp (A.AtExpQuote (_, _, l, _))       = X.genOneExpans l (*(2010-06-24) I don't know if they are actually expansive but I couldn't find the info in the smlnj source code (only looked for 5min) *)
  | nonexpAtExp (A.AtExpDots _)                   = X.Expans []

(** Returns expansiveness of a "SeqExp". *)
and nonexpSeqExp (A.SeqExp (_, _, _, _, l, _)) = X.genOneExpans l
  | nonexpSeqExp (A.SeqExpSl (_, _, _, l, _))  = X.genOneExpans l
  | nonexpSeqExp (A.SeqExpDots _)              = X.Expans []

(** Returns expansiveness of a a labelled expression. *)
and nonexpLabExp (A.LabExp (e, _, _, l, _)) = X.addnonexp (nonexpExp e) l
  | nonexpLabExp (A.LabExpDots _)           = X.Expans []

(** Returns expansiveness of #Ast.ExpField. *)
and nonexpExpField (A.ExpField (_, e, _, _, l, _)) = X.addnonexp (nonexpLabExp e) l
  | nonexpExpField (A.ExpFieldDots _)              = X.Expans []

(** Returns expansiveness of an expression. *)
and nonexpExp (A.ExpAtExp ae)                      = nonexpAtExp ae
  | nonexpExp (A.ExpFn _)                          = X.Nonexp
  | nonexpExp (A.ExpApp (e, ae, _, _, _, l, _))    = X.addnonexp (X.composeNonexp [nonexpConExp e, nonexpAtExp ae]) l
  | nonexpExp (A.ExpCase (_, _, _, _, l, _))       = X.genOneExpans l (* application of a fn *)
  | nonexpExp (A.ExpConsList (_, e1, e2, _, l, _)) = X.addnonexp (nonexpLabExpList [e1, e2]) l
  | nonexpExp (A.ExpOp (_, _, e1, e2, _, l, _))    = X.addnonexp (nonexpLabExpList [e1, e2]) l
  | nonexpExp (A.ExpOr (e1, e2, _, l, _))          = X.addnonexp (nonexpLabExpList [e1, e2]) l
  | nonexpExp (A.ExpAnd (e1, e2, _, l, _))         = X.addnonexp (nonexpLabExpList [e1, e2]) l
  | nonexpExp (A.ExpTyped (e, _, _, l, _))         = X.addnonexp (nonexpLabExp e) l
  | nonexpExp (A.ExpIte (_, _, _, _, l, _))        = X.genOneExpans l (* a case *)
  | nonexpExp (A.ExpWhile (_, _, _, _, l, _))      = X.genOneExpans l (* a let *)
  | nonexpExp (A.ExpRaise (_, _, l, _))            = X.genOneExpans l
  | nonexpExp (A.ExpHandle (_, _, _, l, _))        = X.genOneExpans l
  | nonexpExp (A.ExpDots _)                        = X.Expans []

(** Returns expansiveness of an atomic expression. *)
and nonexpConExp (A.ExpAtExp (A.AtExpParen (e, _, _, l, _))) = X.addnonexp (nonexpDonLabExp e) l
  | nonexpConExp (A.ExpAtExp (A.AtExpId id))                 = nonexpConLongId id
  | nonexpConExp e                                           = X.genMulExpans (toplabExp e)

and nonexpConLabExp (A.LabExp (A.ExpAtExp (A.AtExpParen (e, _, _, l, _)), _, _, k, _)) = X.addnonexp (X.addnonexp (nonexpDonLabExp e) l) k
  | nonexpConLabExp (A.LabExp (A.ExpAtExp (A.AtExpId id), _, _, l, _))                 = X.addnonexp (nonexpConLongId id) l
  | nonexpConLabExp e                                                                  = X.genMulExpans (toplabLabExp e)

and nonexpDonLabExp (A.LabExp (A.ExpTyped (e, _, _, l, _), _, _, k, _)) = X.addnonexp (X.addnonexp (nonexpConLabExp e) l) k
  | nonexpDonLabExp e                                                   = nonexpConLabExp e

(** Returns expansiveness of a structure expression. *)
and nonExpConStrId (A.StrId (_, v, _, l, _)) = SOME (v, l)
  | nonExpConStrId A.StrIdDots               = NONE

(** Returns expansiveness of a #Ast.LongIdQual. *)
and nonexpConLongId (A.LongIdQual (sid, lid, _, lab, _)) =
    (case nonexpConLongId lid of
	 X.Nonexp               => X.Nonexp
       | X.Expans []            => X.Expans []
       | X.Expans [X.Expexp ll] => X.Expans [X.Expexp ll]
       | X.Expans [X.Expdep (id, ll)] =>
	 (case nonExpConStrId sid of
	      NONE => X.Nonexp
	    | SOME (v, l) =>
	      X.Expans [X.Expdep (I.LID ((v, l), id, lab),
				  L.unionsCons [l,lab] [ll])])
       | X.Expans _ =>
	 raise EH.DeadBranch "The expansiveness constraints does not have the expected structure")
  | nonexpConLongId (A.LongIdId id)                      = nonexpConIdent id
  | nonexpConLongId (A.LongIdDots _)                     = X.Expans []

(** Returns expansiveness of an identifier. *)
and nonexpConIdent (A.Ident (_, n, _, l, _)) = X.genOneExpdep n l (* it is expansive if the identifier is a value variable *)
  | nonexpConIdent (A.IdentPcon pc)          = nonexpConPcon pc
  | nonexpConIdent A.IdentDots               = X.Expans []

(** Retuns expansiveness of a A.Pcon* ast constuctors. *)
and nonexpConPcon (A.PconBool _)               = X.Nonexp
  | nonexpConPcon (A.PconNil  _)               = X.Nonexp
  | nonexpConPcon (A.PconRef (_, _, _, l, _))  = X.genOneExpans l
  | nonexpConPcon A.PconDots                   = X.Expans []

and nonexpLabExpList el = X.composeNonexp (map (fn x => nonexpLabExp x) el)
and nonexpExpFieldList el = X.composeNonexp (map (fn x => nonexpExpField x) el)

end
