
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token FIX
%token CONCAT
%token LENGTH
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token LIST
%token STRING
%token AS
%token CASE
%token OF

%token LPAREN
%token RPAREN
%token LCORCH
%token RCORCH
%token LCONE
%token RCONE
%token LBRACKET
%token RBRACKET
%token COMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token OR
%token CASEARROW
%token EOF

%token HEAD
%token TAIL
%token ISNIL 

%token <int> INTV
%token <string> IDV
%token <string> TYIDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    TYIDV EQ LCONE variantty RCONE EOF
        { Type ($1,TyVariant $4) }         
    | IDV EQ term EOF
        { Bind ($1,$3) }   
    | term EOF
        { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix ( TmAbs ($2, $4, $6)), $8)}
  | FIX term
      { TmFix $2 }
  | CASE atomicTerm OF caselist
      {TmCase ($2,$4)}   
      
  
caselist :
    LCONE varianttm RCONE CASEARROW term
        {[(TmVariant ($2,""),$5)]}
    | LCONE varianttm RCONE CASEARROW term OR caselist
        {(TmVariant ($2,""),$5)::$7}        
      
tupletm :
    term COMA term 
      { [$1;$3] }
    | term COMA tupletm 
      { $1::$3 }
                
recordtm :
    IDV EQ term
      { [($1,$3)] }
    | IDV EQ term COMA recordtm
      { ($1,$3)::$5 }
      
      
varianttm :
    IDV EQ term
      { ($1,$3) } 

list : 
    atomicTerm COMA list    
        {TmCons ($1,$3)}   
   |atomicTerm     
        {TmCons ($1,TmNil)}  
   | LBRACKET  RBRACKET COMA list 
        {TmNil}
   | LBRACKET  RBRACKET
        {TmNil}
                       

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }  
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | LENGTH atomicTerm
      { TmLength $2 } 
  |HEAD atomicTerm
      { TmHead $2}
  |TAIL atomicTerm
      { TmTail $2}
  |ISNIL atomicTerm
      { TmIsNil $2}   
  | appTerm atomicTerm
      { TmApp ($1, $2) }
      
      
      
        
        

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }
  | LCORCH tupletm RCORCH
      {TmTuple $2}
  | LCORCH recordtm RCORCH
      {TmRecord $2}
  | LCONE varianttm RCONE AS TYIDV
      {TmVariant ($2,$5)}   
  | LBRACKET list RBRACKET
      { $2 } 
  | atomicTerm DOT INTV
        {TmProjTuple ($1,$3)}          
  | atomicTerm DOT IDV
        {TmProjRecord ($1,$3)}     
  | LBRACKET  RBRACKET     
        {TmNil}    
      
      
                

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LIST  ty
      { TyList ($2)}
      
tuplety :
         ty COMA ty 
                { [$1;$3] }
         | ty COMA tuplety 
                { $1::$3 }
                
recordty :
    IDV COLON ty
      { [($1,$3)] }
    | IDV COLON ty COMA recordty
      { ($1,$3)::$5 }
      
      
variantty :
    IDV COLON ty
      { [($1,$3)] }
    | IDV COLON ty COMA variantty
      { ($1,$3)::$5 }     
                              

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LCORCH tuplety RCORCH
      {TyTuple $2}
  | LCORCH recordty RCORCH
      {TyRecord $2}    
  | LCONE variantty RCONE
      {TyVariant $2}
  | TYIDV
      {TyVar $1} 
         
     
