
(* TYPE DEFINITIONS *)

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


(* CONTEXT MANAGEMENT *)

let empttyctx  =
  []
;;

let emptyvctx =
  []
;;

let addtbinding ctx x bind =
  (x, bind) :: ctx
;;

let addvbinding vctx x term =
  (x, term) :: vctx
;;

exception Bind_error of string
;;

let gettbinding ctx x =
  List.assoc x ctx
;;

let getvbinding ctx x =
  try List.assoc x ctx with _ -> raise (Bind_error x)
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec subty t1 t2= match (t1,t2) with
        (TyArr(x1,x2),TyArr(y1,y2)) -> subty x1 y1 && subty x2 y2
        | (TyRecord l1,TyRecord l2) | (TyVariant l2,TyVariant l1) -> List.for_all (fun (xn,xv) -> List.exists (fun (yn,yv) -> xn=yn && subty xv yv) l2) l1
        | (TyTuple l1, TyTuple l2) -> if List.compare_lengths l1 l2 = 0 then List.equal (fun x -> fun y -> subty x y) l1 l2 else false
        | (TyList x,TyList y) -> subty x y
        | (x,y) -> x=y

let rec string_of_ty tctx ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
       string_of_ty tctx ty1  ^ " -> " ^  string_of_ty tctx ty2 
  | TyString ->
        "String"
  | TyTuple tylist -> (match tylist with
        | h::t -> "{" ^ (List.fold_left (fun x -> fun y -> x ^ "," ^ (string_of_ty tctx y)) (string_of_ty tctx h) t) ^ "}"
        | _ -> "{}")
  | TyRecord tylist -> (match tylist with
        | (n,v)::t -> "{" ^ (List.fold_left (fun x -> fun (y1,y2) -> x ^ ", " ^ y1 ^ " : " ^ (string_of_ty tctx y2)) (n ^ " : " ^ (string_of_ty tctx v)) t) ^ "}"
        | _ -> "{}")
  | TyVariant tylist -> (match tylist with
        | (n,v)::t -> "<" ^ (List.fold_left (fun x -> fun (y1,y2) -> x ^ ", " ^ y1 ^ " : " ^ (string_of_ty tctx y2)) (n ^ " : " ^ (string_of_ty tctx v)) t) ^ ">"
        | _ -> "<>")
  | TyVar t -> (try string_of_ty tctx (gettbinding tctx t) with
        _ -> t)     
  | TyNil ->
      "nil list"
  | TyList t ->
      string_of_ty tctx t ^ " list"           
;;

exception Type_error of string
;;

let rec resolve_tybind tctx ty = match ty with
          TyVar t -> (try (gettbinding tctx t) with _ -> ty)
        | TyArr (t1,t2) -> TyArr ((resolve_tybind tctx t1),(resolve_tybind tctx t2))
        | TyTuple tylist -> TyTuple (List.map (resolve_tybind tctx) tylist)
        | TyRecord tylist -> let (nameList,valList) = List.split tylist in 
                TyRecord (List.combine nameList (List.map (resolve_tybind tctx) valList))
        | TyVariant tylist -> let (nameList,valList) = List.split tylist in 
                TyVariant (List.combine nameList (List.map (resolve_tybind tctx) valList))        
        | TyList t -> TyList (resolve_tybind tctx t)
        | _ -> ty
;;                
        

let rec typeof vctx tctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
       if typeof vctx tctx t1 = TyBool then
        let tyT2 = resolve_tybind tctx (typeof vctx tctx t2) in
        if resolve_tybind tctx (typeof vctx tctx t3) = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if resolve_tybind tctx (typeof vctx tctx t1) = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if resolve_tybind tctx (typeof vctx tctx t1)  = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if resolve_tybind tctx (typeof vctx tctx t1)  = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding tctx x with
       _ -> TyVar x)


      
    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let vctx' = addvbinding vctx x t2 in
      let tctx' = addtbinding tctx x (resolve_tybind tctx tyT1) in
      let tyT2 = resolve_tybind tctx (typeof vctx' tctx' t2) in
      TyArr (tyT1, tyT2)
       
        
        
    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = resolve_tybind tctx (typeof vctx tctx t1) in
      let tyT2 = resolve_tybind tctx (typeof vctx tctx t2) in
      (match tyT1 with
         | TyArr (tyT11, tyT12) ->
             if subty tyT11 tyT2  then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let vctx' = addvbinding vctx x t1 in
      let tyT1 = resolve_tybind tctx (typeof vctx' tctx t1) in
      let tctx' = addtbinding tctx x tyT1 in
      resolve_tybind tctx (typeof vctx' tctx' t2)
    (* T-Fix *)  
  | TmFix t1 ->
      let tyT1 = resolve_tybind tctx (typeof vctx tctx t1) in 
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if subty tyT12 tyT11  then tyT12
          else raise (Type_error "result of body not compatible with domain")
       |_ -> raise (Type_error "arrow type expected") )
       
    (* T-String *)
  | TmString _ ->
      TyString
      
    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if resolve_tybind tctx (typeof vctx tctx t1) = TyString && resolve_tybind tctx (typeof vctx tctx t2)  = TyString then TyString
      else raise (Type_error "argument of concat is not a string")        
      
    (* T-Length *)
  | TmLength t1 ->
      let tyT1 = resolve_tybind tctx (typeof vctx tctx t1) in 
        if tyT1 = TyString then TyNat
        else raise (Type_error "argument of length is not a string")
  
    (*T-Tuple*)
  | TmTuple tmlist -> 
        TyTuple (List.map (typeof vctx tctx) tmlist)
        
        
    (*T-Proj*)    
  | TmProjTuple (term, n) ->
        let tyTerm = resolve_tybind tctx (typeof vctx tctx term)  in
        (match tyTerm with
                TyTuple tylist -> (try List.nth tylist n with _ -> raise (Type_error "Index not in tuple "))
                | t -> raise (Type_error ("the term must be a tuple" ^ string_of_ty tctx t)))
                
  (*T-Record*)
  | TmRecord tmlist -> 
      let (nameList,valList) = List.split tmlist in let rec aux nl = match nl with
                                 h::t -> if List.exists (fun x -> x=h) t then raise (Type_error ("Register with repeating tag " ^ h))
                                         else aux t
                                 | _ -> TyRecord (List.combine nameList (List.map (typeof vctx tctx) valList))
      in aux nameList                           
        
        
    (*T-Proj*)    
  | TmProjRecord (term, name) ->
        let tyTerm = resolve_tybind tctx (typeof vctx tctx term) in
        (match tyTerm with
                TyRecord tylist -> (try List.assoc name tylist with _ -> raise (Type_error ("Record has no tag with name " ^ name)))
                | _ -> raise (Type_error "the term must be a record"))
                
    (*T-Variant*)            
  | TmVariant ((v,term), name) -> (try let t = gettbinding tctx name in
        match t with
        TyVariant tylist ->
                if List.exists (fun (name,ty) -> let tyterm = resolve_tybind tctx (typeof vctx tctx term) in
                                                        match tyterm with
                                                        TyVar _ ->  name = v
                                                        | _ -> name = v &&  tyterm = ty) tylist then t
                else raise (Type_error ("Missmatching parameters"))
        | _ -> raise (Type_error ("type with name " ^ name ^ " is not a variant"))                 
         with Not_found -> raise (Type_error ("no type with name " ^ name)))  
    (*T-Nil*)     
  | TmNil -> 
      TyNil 
    (*T-Cons*)  
  | TmCons (t1,t2) ->
       let tyT1 = typeof vctx tctx t1 in 
       let tyT2 = typeof vctx tctx t2 in (match tyT2 with
          
           TyList(ty) ->
            if tyT1 = ty then TyList(tyT1)
            else raise (Type_error "The type of the terms does not match the type of the list")
          |TyNil -> if tyT1 = TyNil 
            then TyNil
            else TyList(tyT1) 
          |_ ->
            raise (Type_error "The second term must be a list or type nil"))

    (*T-IsNil*)
  | TmIsNil t1 ->
      let tyT1 = resolve_tybind tctx (typeof vctx tctx t1) in 
       (match tyT1 with
            TyList _ -> TyBool
          | TyNil -> TyBool
          | _ ->  raise  (Type_error "The term must be a list"))
    (*T-Head*)
  | TmHead t1 -> 
      let tyT1 = resolve_tybind tctx (typeof vctx tctx t1) in
      (match tyT1 with
        TyList ty2 -> TyList ty2
       |_ -> raise (Type_error "The term must be a list") ) 
    (*T-Tail*)
  | TmTail t1 -> 
    let tyT1 = resolve_tybind tctx (typeof vctx tctx t1) in
    (match tyT1 with
      TyList ty2 -> TyList ty2
     |_ -> raise (Type_error "The term must be a list") )
    (*T-Case*)
  | TmCase (t,t2) -> (match t with
        TmVariant _ | TmVar _ -> (let tyt = resolve_tybind tctx (typeof vctx tctx t) in
                (match tyt with 
                        TyVariant tyl ->let aux (t1,t2)= (match t1 with
                                | TmVariant ((n1,TmVar t),_) -> 
                                         let tctx' = addtbinding tctx t (List.assoc n1 tyl) in
                                         (try resolve_tybind tctx (typeof vctx tctx' t2)
                                                 with _ -> raise (Type_error "Unexpected error TmCase"))
                                | TmVariant _ -> 
                                         (try resolve_tybind tctx (typeof vctx tctx t2)
                                                 with _ -> raise (Type_error "Unexpected error TmCase"))
                                | _ -> raise (Type_error "Unexpected error TmCase"))
                        in (match t2 with
                                h::tail -> (let tyh = aux h in 
                                        if List.for_all (fun x -> tyh = aux x) tail
                                        then tyh
                                        else raise (Type_error "Case branches have different types"))
                                | _ -> raise (Type_error "Empty case branches"))
                        | _ -> raise (Type_error "Unexpected error TmCase")))
                         
        | _ -> raise (Type_error "Case term must be a variant") )
   
          
                                       
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term tctx = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "\n\tif : "  ^ string_of_term tctx t1  ^
      "\n\t  then : "  ^ string_of_term tctx t2 ^
      "\n\t  else : "  ^ string_of_term tctx t3 
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ ": " ^ string_of_term tctx t 
      in f 1 t
  | TmPred t ->
      "pred " ^ ": " ^ string_of_term tctx t 
  | TmIsZero t ->
      "iszero " ^ ": " ^ string_of_term tctx t 
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "lambda " ^ s ^ " : " ^ string_of_ty tctx tyS ^ ". " ^ string_of_term tctx t 
  | TmApp (t1, t2) ->
       string_of_term tctx t1 ^ " " ^ string_of_term tctx t2  
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term tctx t1 ^ " in " ^ string_of_term tctx t2
  | TmFix t -> 
      "fix :" ^string_of_term tctx t 
  | TmString s -> 
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) -> 
      "concat : "  ^ string_of_term tctx t1 ^ " <-> " ^ string_of_term tctx t2
  | TmLength t -> 
      "length : " ^ string_of_term tctx t 
  | TmTuple tmlist -> (match tmlist with
        | h::t -> "{" ^ (List.fold_left (fun x -> fun y -> x ^ "," ^ (string_of_term tctx y)) (string_of_term tctx h) t) ^ "}"
        | _ -> "{}")
  | TmProjTuple (term, n) ->
        string_of_term tctx term ^ "." ^ (string_of_int n)
  | TmRecord tmlist -> (match tmlist with
        | (n,v)::t -> "{" ^ (List.fold_left (fun x -> fun (y1,y2) -> x ^ ", " ^ y1 ^ " = " ^ (string_of_term tctx y2)) (n ^ " = " ^ (string_of_term tctx v)) t) ^ "}"
        | _ -> "{}") 
  | TmProjRecord (term, name) ->
        string_of_term tctx term ^ "." ^ name
  | TmVariant ((name,term),_) -> 
         "<" ^ name ^ " = " ^ (string_of_term tctx term) ^ ">"
        
  | TmNil ->
          "[]" 
  | TmIsNil t -> 
          "isnil :" ^ string_of_term tctx t  
  | TmCons (t1,t2)->
      let rec sotAux t1 = match t1 with
         TmCons (TmNil,TmNil) -> "]"
        |TmCons (v1 ,TmNil) -> string_of_term tctx v1 ^ "]"
        |TmCons (v1,v2) -> string_of_term tctx v1  ^ "," ^ sotAux v2
        | _ -> "Invalid"
         in "[" ^ ( sotAux (TmCons (t1,t2)) )  
         
  | TmHead t1 ->
           "head : " ^ string_of_term tctx t1 
  | TmTail t1 ->
           "tail : " ^ string_of_term tctx t1   

  | TmCase (t,t2) -> 
           "case " ^ string_of_term tctx t ^ " of\n" ^
           (match t2 with
           (h1,h2)::t -> List.fold_left (fun x -> fun (y1,y2) -> x ^ "\n| " ^ (string_of_term tctx y1) ^ " => " ^ (string_of_term tctx y2))
                (" " ^ (string_of_term tctx h1) ^ " => " ^ (string_of_term tctx h2)) t
           | _ -> "" )         
                                       
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
    free_vars t
  | TmString t ->
    [t]
  | TmConcat (t1, t2) ->
    lunion (free_vars t1) (free_vars t2) 
  | TmLength t ->
    free_vars t
  | TmTuple tmlist ->
        List.fold_right (fun x -> fun l -> lunion x l) (List.map free_vars tmlist) []
  | TmProjTuple (term, _) ->
        free_vars term
  | TmRecord tmlist -> let (_,valList) = List.split tmlist in 
        List.fold_right (fun x -> fun l -> lunion x l) (List.map free_vars valList) []
  | TmProjRecord (term, _) ->
        free_vars term
  | TmVariant ((_,term),_) -> 
        free_vars term 
   |TmNil ->
          []
   | TmIsNil t ->
         free_vars t 
   | TmCons (t1,t2) ->
          lunion (free_vars t1) (free_vars t2) 
   | TmHead t->
          free_vars t 
   | TmTail t -> 
          free_vars t
   | TmCase (_,t2) ->  List.fold_right (fun (_,x2) -> fun l -> lunion (free_vars x2) l) t2 []             
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmLength t ->
      TmLength (subst x s t)
  | TmTuple tmlist ->
      TmTuple (List.map (subst x s) tmlist)
  | TmProjTuple (term, n) -> 
        TmProjTuple ((subst x s term),n)
  | TmRecord tmlist -> let (nameList,valList) = List.split tmlist in 
      TmRecord (List.combine nameList (List.map (subst x s) valList))
  | TmProjRecord (term, name) ->
        TmProjRecord ((subst x s term),name)
  | TmVariant ((v,term),name) -> 
      TmVariant  ((v,(subst x s term)),name) 
  |TmNil ->
     TmNil
  | TmIsNil t -> 
      TmIsNil (subst x s t)
  | TmCons (t1,t2)->
      TmCons ( subst x s t1, subst x s t2)
  | TmHead t -> 
      TmHead (subst x s t)
  | TmTail t -> 
      TmTail (subst x s t)
  | TmCase (t,t2) ->  let (term1List,term2List) = List.split t2 in   
       TmCase ((subst x s) t,(List.combine (List.map (subst x s) term1List) (List.map (subst x s) term2List)))                                  
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmVariant _ -> true
  | TmTuple _ -> true
  | TmRecord _ -> true 
  | t when isnumericval t -> true
  | TmCons (TmNil,TmNil) -> false
  | TmCons _ -> true
  | _ -> false
;;

exception NoRuleApplies
;;

exception Evaluation_error of string
;;

let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

      
    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12
      

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')
      

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta *)  
  | TmFix (TmAbs (x, _, t2 )) -> 
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 -> 
      let t1' = eval1 vctx t1 in 
      TmFix t1'
      
  | TmVar y ->
    getvbinding vctx y

    (* E-Concat *)
  | TmConcat (TmString s1, TmString s2) -> 
      TmString (s1 ^ s2)
      
    (* E-Concat *)
  | TmConcat (TmString s1, t2) -> 
      let t2' = eval1 vctx t2 in 
      TmConcat (TmString s1, t2')
      
    (* E-Concat *)
  | TmConcat (t1, t2) -> 
      let t1' = eval1 vctx t1 in 
      TmConcat (t1', t2)           
      
    (* E-Length *)
  | TmLength (TmString s1) ->
         if s1 = "" then TmZero
         else TmSucc (eval1 vctx (TmLength (TmString (String.sub s1 0 (String.length s1 - 1)))))
              
    (* E-Length *)
  | TmLength t1 ->      
      let t1' = eval1 vctx t1 in 
      TmLength t1'
      
      (*E-Tuple*)
  | TmTuple tmlist ->
        TmTuple (List.map (eval1 vctx) tmlist)
        
      (*E-Proj*)
  | TmProjTuple (TmTuple tmlist, n) ->
        List.nth tmlist n
                
        (*E-Proj*)
  | TmProjTuple (term, n) ->
        let term' = eval1 vctx term in
        TmProjTuple (term', n)             
                
      (*E-Record*)
  | TmRecord tmlist -> let (nameList,valList) = List.split tmlist in 
      TmRecord (List.combine nameList (List.map (eval1 vctx) valList))
        
                
        (*E-Proj*)
  | TmProjRecord (TmRecord tmlist, name) ->
        List.assoc name tmlist
                
        (*E-Proj*)
  | TmProjRecord (term, name) ->
        let term' = eval1 vctx term in
        TmProjRecord (term', name)              
                
      (*E-Variant*)
  | TmVariant ((v,term),name) -> 
        TmVariant ((v,(eval1 vctx term)),name)
      (* E-Cons2*)
  | TmCons (t1,t2) when isval t1 -> 
      let t2' = eval1 vctx t2 in 
      TmCons(t1,t2')

      (* E-Cons1*)
  | TmCons (t1,t2) ->
      let t1' = eval1 vctx t1 in 
      TmCons(t1', t2)
    (*E-IsNilNil*)
  | TmIsNil (TmNil) ->
      TmTrue
    (*E-IsNilCons*)
  | TmIsNil t when isval t -> 
      TmFalse
    (* E-IsNil*) 
  | TmIsNil t1 ->
      let t1' = eval1 vctx t1 in
        TmIsNil t1'
    (*E-HeadCons*)
  | TmHead (TmCons (v1,v2)) ->
      v1
    (*E-Head*)
  | TmHead t1 ->
     let t1' = eval1 vctx t1 in 
       TmHead t1'
    (*E-TailCons*) 
  | TmTail (TmCons (v1,v2)) ->
      v2
    (*E-Tail*)
  | TmTail t1 -> 
     let t1' = eval1 vctx t1 in 
      TmTail t1'
      
  (*E-Case*)  
  | TmCase (TmVariant ((name,v),_),t2) -> let rec aux t2 = match t2 with
        (TmVariant ((h1name,h1term),_),h2)::t -> if name=h1name then (match h1term with
                                                                TmVar t -> subst t v h2
                                                                | _ -> if h1term = v then h2 else aux t)
                                                      else aux t 
        | _ -> raise (Evaluation_error "No pattern matches")
        in aux t2
                                                               
 
    (*E-Case*)  
  | TmCase (t,t2) -> 
        TmCase ((eval1 vctx t),t2)
                                            
  | _ -> 
      raise NoRuleApplies
;;

let apply_ctx vctx tm =
  let rec aux vl = function
    | TmTrue -> 
        TmTrue
    | TmFalse ->
        TmFalse
    | TmIf (t1, t2, t3) ->
       TmIf (aux vl t1, aux vl t2, aux vl t3)
    | TmZero ->
      TmZero
    | TmSucc t ->
      TmSucc (aux vl t)
    | TmPred t ->
      TmPred (aux vl t)
    | TmIsZero t ->
      TmIsZero (aux vl t)
    | TmVar y ->
      if List.mem y vl then TmVar y else getvbinding vctx y
    | TmAbs (s, t, t1) -> 
      TmAbs (s, t, aux (s::vl) t1)
    | TmApp (t1, t2) ->
      TmApp (aux vl t1, aux vl t2)
    | TmLetIn (y, t1, t2) ->
      TmLetIn (y, aux vl t1, aux (y::vl) t2)
    | TmFix t ->
      TmFix (aux vl t)
    | TmString t ->
      TmString t
    | TmConcat (t1, t2) ->
      TmConcat (aux vl t1, aux vl t2)
    | TmLength t ->
      TmLength (aux vl t)
    | TmTuple tmlist ->
      TmTuple  (List.map (aux vl) tmlist)
    | TmProjTuple (term, n) ->
        TmProjTuple ((aux vl term), n)
    | TmRecord tmlist -> let (nameList,valList) = List.split tmlist in 
      TmRecord (List.combine nameList (List.map (aux vl) valList))
    | TmProjRecord (term, name) ->
        TmProjRecord ((aux vl term), name)
    | TmVariant ((v,term),name) -> 
        TmVariant ((v,(aux vl term)),name)
    | TmNil ->
      TmNil
    |TmIsNil t ->
      TmIsNil (aux vl t)

    |TmCons (t1,t2) ->
      TmCons (aux vl t1, aux vl t2)
    |TmHead t -> 
      TmHead (aux vl t)

    |TmTail t ->
      TmTail (aux vl t)
      
    |TmCase(t,t2) ->
      TmCase((aux vl t),t2)     
                                                     
  in aux [] tm
;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;
