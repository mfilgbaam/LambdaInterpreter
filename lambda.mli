
type ty =
TyBool
| TyNat
| TyArr of ty * ty
| TyString
| TyTuple of ty list
| TyRecord of (string * ty) list
| TyVariant of (string * ty) list
| TyVar of string
| TyNil
| TyList of ty 
;;

type tcontext =
(string * ty) list
;;

type term =
TmTrue
| TmFalse
| TmIf of term * term * term
| TmZero
| TmSucc of term
| TmPred of term
| TmIsZero of term
| TmVar of string
| TmAbs of string * ty * term
| TmApp of term * term
| TmLetIn of string * term * term
| TmFix of term
| TmString of string
| TmConcat of term * term
| TmLength of term
| TmTuple of term list 
| TmProjTuple of (term * int)
| TmRecord of (string * term) list
| TmProjRecord of (term * string)
| TmVariant of (string * term) * string
| TmNil
| TmCons of term * term
| TmIsNil of term
| TmHead of term
| TmTail of term
| TmCase of term * (term * term) list
;;

type vcontext = 
  (string * term) list
;;

type command = 
  Eval of term
  | Bind of string * term
  | Type of string * ty
;;

val empttyctx : tcontext;;
val addtbinding : tcontext -> string -> ty -> tcontext;;
val gettbinding : tcontext -> string -> ty;;

val emptyvctx : vcontext;;
val addvbinding : vcontext -> string -> term -> vcontext;;
val getvbinding : vcontext -> string -> term;;

val subty : ty -> ty -> bool;;

val string_of_ty : tcontext -> ty -> string;;
exception Type_error of string;;
val resolve_tybind : tcontext -> ty -> ty;;
val typeof : vcontext -> tcontext -> term -> ty;;

val string_of_term : tcontext -> term -> string;;
exception NoRuleApplies;;
exception Evaluation_error of string;;
val eval : vcontext -> term -> term;;
