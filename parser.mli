type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | FIX
  | CONCAT
  | LENGTH
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | LIST
  | STRING
  | AS
  | CASE
  | OF
  | LPAREN
  | RPAREN
  | LCORCH
  | RCORCH
  | LCONE
  | RCONE
  | LBRACKET
  | RBRACKET
  | COMA
  | DOT
  | EQ
  | COLON
  | ARROW
  | OR
  | CASEARROW
  | EOF
  | HEAD
  | TAIL
  | ISNIL
  | INTV of (
# 50 "parser.mly"
        int
# 47 "parser.mli"
)
  | IDV of (
# 51 "parser.mly"
        string
# 52 "parser.mli"
)
  | TYIDV of (
# 52 "parser.mly"
        string
# 57 "parser.mli"
)
  | STRINGV of (
# 53 "parser.mly"
        string
# 62 "parser.mli"
)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
