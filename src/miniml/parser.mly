%{
  open Syntax
%}
(*parser takes the token stream from the lexer and checks whether the tokens conform to the language’s grammar rules. 
example: let x = 5 + 10 -> Lexer -> LET, VAR("x"), EQUAL, INT(5), PLUS, INT(10)
parser -> match the grammar rule let_expr → "let" VAR "=" expr*)
(*We process the tokens which the lexer sends using the %token token_name code
I have added functionality for LBRACE, RBRACE, PIPE, DIVIDE, TRY, WITH, RAISE, DIVISIONBYZERO, GENERICEXCEPTION*)
%token TINT
%token TBOOL
%token TARROW
%token <Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS
%token MINUS
%token TIMES
%token EQUAL LESS
%token IF THEN ELSE
%token FUN IS
%token COLON
%token LPAREN RPAREN
%token LBRACE RBRACE
%token PIPE
%token LET
%token SEMISEMI
%token EOF
%token TRY (*try expr with DivisionByZero -> 100*)
%token WITH (*try expr with DivisionByZero -> 100*)
%token RAISE (*raise DivisionByZero*)
%token DIVIDE (*x / y*)
%token DIVISIONBYZERO (*raise DivisionByZero*)
%token GENERICEXCEPTION (*raise GenericException(42)*)
%token <string> STRING (*This token is matched whenever the lexer encounters a string literal in the input, such as "Hello, World!"*)

%start file
%type <Syntax.command list> file

%start toplevel
%type <Syntax.command> toplevel

%nonassoc IS
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES
%right TARROW

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = nonempty_list(def) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(def) EOF
    { ds }

toplevel:
  | d = def SEMISEMI
    { d }
  | e = expr SEMISEMI
    { Expr e }

def:
  | LET x = VAR EQUAL e = expr
    { Def (x, e) }
(*Since, we need to deal with exceptions, we have the exception_pattern code as given below to handle the different
cases for the 2 exceptions DIVISIONBYZERO and GENERICEXCEPTION(INT)*)    
exception_pattern:
(* Handles exception patterns for DivisionByZero and GenericException, with priority given to parenthesized versions 
to resolve ambiguity. Refer test_exn.ml for examples with paranthesis*)
  | LBRACE ep = exception_pattern RBRACE
    { ep }  (* Exception pattern in braces *)
  | LPAREN ep = exception_pattern RPAREN
    { ep }  (* Parenthesized exception pattern *)
  | DIVISIONBYZERO LBRACE RBRACE
    { DivisionByZero } (*To handle DivisionByZero in {}*)
  | DIVISIONBYZERO LPAREN RPAREN
    { DivisionByZero } (*To handle DivisionByZero in ()*)
  | DIVISIONBYZERO
    { DivisionByZero } (*To handle DivisionByZero without any braces*)
  | GENERICEXCEPTION LBRACE n = INT RBRACE
    { GenericException n } (*To handle GenericException{n} for positive n*)
  | GENERICEXCEPTION LBRACE MINUS n = INT RBRACE
    { GenericException (-n) } (*To handle GenericException{n} for negative n*)
  | GENERICEXCEPTION LPAREN n = INT RPAREN
    { GenericException n } (*To handle GenericException(n) for positive n*)
  | GENERICEXCEPTION LPAREN MINUS n = INT RPAREN
    { GenericException (-n) } (*To handle GenericException(n) for negative n*)
  | GENERICEXCEPTION n = INT 
  (*Order matters: Put the parenthesized version before the non-parenthesized one to avoid ambiguity.*)
    { GenericException n } (*To handle GenericException n for positive n*)
  | GENERICEXCEPTION MINUS n = INT 
  (*Order matters: Put the parenthesized version before the non-parenthesized one to avoid ambiguity.*)
    { GenericException (-n) } (*To handle GenericException n for negative n*)
(*The code below defines how to parse a plain expression (any simple/basic expression in miniml).*)   
expr: mark_position(plain_expr) { $1 }
plain_expr:
  | e = plain_app_expr
    { e }
  | MINUS n = INT
    { Int (-n) }
  | e1 = expr PLUS e2 = expr	
    { Plus (e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Minus (e1, e2) }
  | e1 = expr TIMES e2 = expr
    { Times (e1, e2) }
  | e1 = expr EQUAL e2 = expr
    { Equal (e1, e2) }
  | e1 = expr LESS e2 = expr
    { Less (e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  (*To handle division operation between two numbers, raising and handling exceptions like DivisionByZero and
  GenericException(int n) (for both positive and negative integers n).*)
  | e1 = expr DIVIDE e2 = expr (*Creates a Divide expression from two subexpressions (e1 and e2).*)
    { Divide (e1, e2) }
  | RAISE DIVISIONBYZERO LBRACE RBRACE   (*Creates a Raise expression for DivisionByZero.*)
    { Raise DivisionByZero }
  | RAISE DIVISIONBYZERO LPAREN  RPAREN   (*Creates a Raise expression for DivisionByZero.*)
    { Raise DivisionByZero }
  | RAISE DIVISIONBYZERO
    { Raise DivisionByZero }
  (*Order matters: Put the parenthesized version before the non-parenthesized one*)
  | RAISE GENERICEXCEPTION LBRACE n = INT RBRACE
    { Raise (GenericException n) } (*For cases with GenericException{n}, n is a positive integer*)
  (*Handle negative values for GenericException{-32}*)
  | RAISE GENERICEXCEPTION LBRACE MINUS n = INT RBRACE
    { Raise (GenericException (-n)) } (*For cases with GenericException{n}, n is a negative integer*)
  (*Order matters: Put the parenthesized version before the non-parenthesized one*)
  (*Handle negative values for GenericException{-32}*)
  | RAISE GENERICEXCEPTION LPAREN n = INT RPAREN
    { Raise (GenericException n) } (*For cases with GenericException(n), n is a positive integer*)
  (*Handle negative values for GenericException(-32)*)
  | RAISE GENERICEXCEPTION LPAREN MINUS n = INT RPAREN
    { Raise (GenericException (-n)) } (*For cases with GenericException(n), n is a positive integer*)
  | RAISE GENERICEXCEPTION n = INT 
    { Raise (GenericException n) } (*For cases with GenericException n, n is a positive integer*)
  | RAISE GENERICEXCEPTION MINUS n = INT 
    { Raise (GenericException (-n)) } (*For cases with GenericException n, n is a positive integer*)
  | RAISE LBRACE exn = exception_pattern RBRACE
  (* RAISE with any exception pattern: Raises the specified exception
  (could be DivisionByZero or GenericException with argument.*)
    { Raise exn }  (* try raise {GenericException n} with  GenericException n -> true;; *)
  | RAISE LPAREN exn = exception_pattern RPAREN
  (* RAISE with any exception pattern: Raises the specified exception 
  (could be DivisionByZero or GenericException with argument.*)
    { Raise exn }  (* try raise (GenericException n) with  GenericException n -> true;; *)
  | TRY e1 = expr WITH LBRACE ep=exception_pattern TARROW e2 = expr RBRACE
    { TryWith (e1, ep, e2) }  (*For cases like try e1 with {Exception -> e2}*)
  | TRY e1 = expr WITH LBRACE PIPE ep=exception_pattern TARROW e2 = expr RBRACE
  { TryWith (e1, ep, e2) }  (*For cases like try e1 with { | Exception -> e2}*)
  | TRY e1 = expr WITH PIPE LBRACE ep=exception_pattern TARROW e2 = expr RBRACE
  { TryWith (e1, ep, e2) }  (*For cases like try e1 with | {Exception -> e2}*)
  | TRY e1 = expr WITH LPAREN ep=exception_pattern TARROW e2 = expr RPAREN
    { TryWith (e1, ep, e2) }  (*For cases like try e1 with (Exception -> e2)*)
  | TRY e1 = expr WITH LPAREN PIPE ep=exception_pattern TARROW e2 = expr RPAREN
    { TryWith (e1, ep, e2) }  (*For cases like try e1 with ( | Exception -> e2)*)
  | TRY e1 = expr WITH PIPE LPAREN ep=exception_pattern TARROW e2 = expr RPAREN
    { TryWith (e1, ep, e2) }  (*For cases like try e1 with | (Exception -> e2)*)
  | TRY e1 = expr WITH ep=exception_pattern TARROW e2 = expr
    { TryWith (e1, ep, e2) }  (*For cases like try e1 with Exception -> e2*)
  | TRY e1 = expr WITH PIPE ep=exception_pattern TARROW e2 = expr
    { TryWith (e1, ep, e2) }  (*For cases like try e1 with | Exception -> e2*)
  | FUN x = VAR LBRACE f = VAR COLON t1 = ty RBRACE COLON t2 = ty IS e = expr
    { Fun (x, f, t1, t2, e) }  (*Dealing with expressions having {}*)
  | FUN x = VAR LPAREN f = VAR COLON t1 = ty RPAREN COLON t2 = ty IS e = expr
    { Fun (x, f, t1, t2, e) }  

app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { Apply (e1, e2) }

simple_expr: mark_position(plain_simple_expr) { $1 }
plain_simple_expr:
  | x = VAR
    { Var x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | LBRACE e = plain_expr RBRACE
    { e } (*Implementation of braces in plain simple expressions*)
  | LPAREN e = plain_expr RPAREN	
    { e }    

ty:
  | TBOOL
    { TBool }
  | TINT
    { TInt }
  | t1 = ty TARROW t2 = ty
    { TArrow (t1, t2) }
  | LBRACE t = ty RBRACE
    { t } (*Implementation of braces in types*)
  | LPAREN t = ty RPAREN
    { t }

mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%

