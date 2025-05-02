{
  open Parser
}

(*let x = 5 + 3 ;; is broken down to 
let → keyword x → identifier = → operator 5 → integer + → operator 3 → integer ;; → statement separator*)
(*This line of code makes sure that any variable defined in the commands can have both lower and upper 
case alphabets, underscore, numbers in their names*)
let var = ['a'-'z' 'A'-'Z' '_' '0'-'9']+
(*This set of code tells the lexer to tokenize the given keywords in the left as a specific name in the right. 
Ex: if we say let x = 0 is parsed as LET (KEYWORD), x (IDENTIFIER), EQUAL (KEYWORD), INT 0 (KEYWORD)*)
(*I have now added the following keywords: LBRACE ({), RBRACE(}), PIPE(|), DIVIDE (/), TRY (try), WITH (with),
RAISE (raise), DIVISIONBYZERO (DivisionByZero), GENERICEXCEPTION (GenericException)*)
rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+      { INT (int_of_string(Lexing.lexeme lexbuf)) }
(* This does not work for n*fact(n-1) kind of expressions *)
(*  | '-' ['0'-'9']+  { INT (int_of_string(Lexing.lexeme lexbuf)) } *) (* Allow negative numbers *)
  | "int"           { TINT }
  | "bool"          { TBOOL }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "is"            { IS }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }  
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  | '<'             { LESS }
  | "->"            { TARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '|'             { PIPE }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE } (* Matches the division operator '/' and returns the token DIVIDE. *)
  | "try"            { TRY }  (* Matches the keyword "try" and returns the token TRY. This is typically used in exception handling. *)
  | "with"           { WITH }  (* Matches the keyword "with" and returns the token WITH. This is used in combination with "try" for exception handling. *)
  | "raise"          { RAISE } (* Matches the keyword "raise" and returns the token RAISE. This is used to raise an exception. *)
  | "DivisionByZero"      { DIVISIONBYZERO }   (* Matches the keyword "DivisionByZero" and returns the token DIVISIONBYZERO. It represents a specific exception. *)
  | "GenericException"    { GENERICEXCEPTION }  (* Matches the keyword "GenericException" and returns the token GENERICEXCEPTION. This represents a generic exception type. *)
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}
