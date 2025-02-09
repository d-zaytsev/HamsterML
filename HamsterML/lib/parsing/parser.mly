%{
    open Ast    
%}

// --- Tokens ---

// Data Types
%token <int> TYPE_INT
%token <float> TYPE_FLOAT
%token <bool> TYPE_BOOL
%token <char> TYPE_CHAR
%token <string> TYPE_STRING
%token TYPE_UNIT

// Param Types
%token INT
%token FLOAT
%token BOOL
%token CHAR
%token STRING
%token <string> POLYMORPHIC_NAME // (x: `a) (y: `b)

%token <string> IDENTIFIER

// Statements 
%token IF
%token THEN
%token ELSE
%token FUN          // "fun _ -> _"
%token LET
%token IN
%token REC
%token MATCH
%token WITH
%token WILDCARD     // "_"

// Separators
%token ARROW        // "->"
%token COMMA        // "."
%token SEMICOLON    // ";"
%token COLON        // ":"
%token DOUBLE_COLON // "::"
%token BAR          // "|"

// braces
%token LEFT_PARENTHESIS       // "("
%token RIGHT_PARENTHESIS      // ")"
%token LEFT_SQ_BRACKET        // "["
%token RIGHT_SQ_BRACKET       // "]"

// Operators
%token PLUS                 // "+"
%token MINUS                // "-"
%token ASTERISK             // "*"
%token SLASH                // "/"
%token CARET                // "^"
%token EQUAL                // "="
%token NOT_EQUAL            // "!=" || "<>" TODO: check semantics
%token GREATER_THAN         // ">"
%token GREATER_THAN_EQUAL   // ">="
%token LESS_THAN            // "<"
%token LESS_THAN_EQUAL      // "<"
%token LET_AND              // "let x = 1 and y = 2" 
%token AND                  // "&&"
%token OR                   // "||"
%token NOT                  // "not"

// End Of File
%token EOF

// --- Priorities ---
%left OR
%left AND
%left NOT

%left GREATER_THAN_EQUAL
%left LESS_THAN_EQUAL
%left GREATER_THAN
%left LESS_THAN
%left NOT_EQUAL
%left EQUAL

%left CARET
%left PLUS, MINUS
%left ASTERISK, SLASH

// --- Parsing ---
%start <Ast.expr> prog
%%

prog : p = expr EOF { p }

expr:
    | LEFT_PARENTHESIS; e = expr; RIGHT_PARENTHESIS                     { e }
    | v = value                                                         { Value v }
    | uop = uop; e = expr                                               { UnOp (uop, e) }
    | le = expr; bop = bop; re = expr                                   { BinOp (bop, le, re) }
    | LET; REC; l = let_rec_def                                         { l }
    | LET; l = let_in_def                                               { l }
    | LET; l = let_def                                                  { l }
    | MATCH; expr = expr; WITH; match_cases = nonempty_list(match_case) { Match (expr, match_cases) }
    | FUN; vls = nonempty_list(value); ARROW; e = expr                  { Fun (vls, e) }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr                   { If (e1, e2, Some e3) }
    | IF; e1 = expr; THEN; e2 = expr                                    { If (e1, e2, None) }
    | le = expr; re = expr                                              { Application (le,re) }

dataType:
    | i = TYPE_INT      {Int i}
    | f = TYPE_FLOAT    {Float f}
    | b = TYPE_BOOL     {Bool b}
    | c = TYPE_CHAR     {Char c}
    | s = TYPE_STRING   {String s}
    | TYPE_UNIT         { Unit }

%inline identifier:
    | name = IDENTIFIER { name }

value:
    | LEFT_PARENTHESIS; v = value; RIGHT_PARENTHESIS    { v }
    | WILDCARD                                          {Wildcard}
    | t_v = typed_var                                   { t_v }
    | v = const_or_var                                  { v }
    | p = pattern                                       { p }

pattern:
    | tpl = tuple_dt                                {Tuple tpl} (* (1, 2, 3, ...) *)
    | lst = list_dt                                 {List lst}  (* [1; 2; 3; ...] *)
    | v = const_or_var ; DOUBLE_COLON; l = list_dt  { ListConcat (v, List l) }

%inline paramType:
    | INT                   { PInt }
    | FLOAT                 { PFloat }
    | BOOL                  { PBool }
    | CHAR                  { PChar }
    | STRING                { PString }
    | n = POLYMORPHIC_NAME  { Poly n }

%inline bop:
    | PLUS                  { ADD }                 
    | MINUS                 { SUB }        
    | ASTERISK              { MUL }                  
    | SLASH                 { DIV }                  
    | CARET                 { CONCAT }               
    | EQUAL                 { EQ }                
    | NOT_EQUAL             { NEQ }           
    | GREATER_THAN          { GT }       
    | GREATER_THAN_EQUAL    { GTE }  
    | LESS_THAN             { LT }      
    | LESS_THAN_EQUAL       { LTE }     
    | AND                   { AND }            
    | OR                    { OR }            

%inline uop:
    | NOT { NOT }   

const_or_var: (* Const or variable *)
    | const = dataType      {Const const} 
    | varId = identifier    { VarId varId }

typed_var: typedVarId = identifier; COLON; varType = paramType {TypedVarID (typedVarId, varType) }

%inline tuple_dt: LEFT_PARENTHESIS; val_list = separated_nonempty_list(COMMA, value); RIGHT_PARENTHESIS { val_list }
%inline list_dt: LEFT_SQ_BRACKET; val_list = separated_list(SEMICOLON, value); RIGHT_SQ_BRACKET         { val_list }

%inline match_case: 
    | BAR; v = value; ARROW; e = expr { (v, e) }

(* <id> <value1> <value2> ... = <expr> *)
%inline let_rec_def: id = IDENTIFIER; vls = list(value); EQUAL; e = expr    { Let (Recursive, id, vls, e) }
%inline let_def: id = IDENTIFIER; vls = list(value); EQUAL; e = expr        { Let (Nonrecursive, id, vls, e) }

let_bind: (* a = 10 and b = 20 and ... *)
    | l = let_def                           { [l] }
    | h = let_def; LET_AND; tl = let_bind   { h :: tl }

let_in_def: 
    | e1 = let_def; IN; e2 = expr   { LetIn ([e1], e2) } (* without and *)
    | exs = let_bind; IN; e = expr  { LetIn (exs, e) } (* with and *)