
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "fix"       { FIX }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "Str"       { STRING } 
  | "List"      { LIST }
  | "list"      { LIST }
  | "concat"    { CONCAT }
  | "length"    { LENGTH }
  | "len"       { LENGTH }  
  | "as"        { AS }   
  | "case"      { CASE }
  | "of"        { OF }   
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "isnil"     { ISNIL }   
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LCORCH }
  | '}'         { RCORCH }
  | '['         { LBRACKET}
  | ']'         { RBRACKET}
  | '<'         { LCONE}
  | '>'         { RCONE}
  | ','         { COMA }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "|"         { OR }
  | "=>"        { CASEARROW}  
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['a'-'z' '_' '0'-'9']*
                { TYIDV (Lexing.lexeme lexbuf) }                
  | '"'[^ '"' ';' '\n']*'"'
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s - 2)) }
  | eof         { EOF }
  | _           { raise Lexical_error }
