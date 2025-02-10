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

// Explicit Types
%token INT
%token FLOAT
%token BOOL
%token CHAR
%token STRING

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
%token IDENTICAL_EQ         // "=="
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
%left IDENTICAL_EQ

%left CARET
%left PLUS, MINUS
%left ASTERISK, SLASH

// --- Parsing ---
%type <expr list> prog
%start prog
%%

// --- Subs ---

%inline paramType:
    | INT                   { PInt }
    | FLOAT                 { PFloat }
    | BOOL                  { PBool }
    | CHAR                  { PChar }
    | STRING                { PString }

%inline bop:
    | PLUS                  { ADD }                 
    | MINUS                 { SUB }        
    | ASTERISK              { MUL }                  
    | SLASH                 { DIV }                  
    | CARET                 { CONCAT }               
    | EQUAL                 { EQ }       
    | IDENTICAL_EQ          { ID_EQ }         
    | NOT_EQUAL             { NEQ }           
    | GREATER_THAN          { GT }       
    | GREATER_THAN_EQUAL    { GTE }  
    | LESS_THAN             { LT }      
    | LESS_THAN_EQUAL       { LTE }     
    | AND                   { AND }            
    | OR                    { OR }            

%inline uop:
    | NOT { NOT }

dataType:
    | f = float             { f }
    | i = int               { i }
    | b = TYPE_BOOL         { Bool b }
    | c = TYPE_CHAR         { Char c }
    | s = TYPE_STRING       { String s }
    | TYPE_UNIT             { Unit }

int:
    | i = TYPE_INT              {Int i}
    | MINUS; i = TYPE_INT       {Int (-i)}
    | PLUS; i = TYPE_INT        {Int (i)}

float:
    | f = TYPE_FLOAT            {Float f}
    | MINUS; f = TYPE_FLOAT     {Float (-.f)}
    | PLUS; f = TYPE_FLOAT      {Float (f)}

// --- Parser rules ---

prog : e = expr EOF { [e] }

// declaration:
//     | LET; l = let_def                                                  { l }
//     | LET; l = let_and_in_def                                           { l } 

typ_opt:
    | COLON; t = paramType { Some t }
    | { None }

expr:
    | t = tuple_pattern { Pattern t }
    | LEFT_PARENTHESIS; e = expr; RIGHT_PARENTHESIS                     { e }
    | e = operation_expr                                                { e }
    | e = if_expr                                                       { e }
    | p = pattern                                                       { Pattern p }
    | le = expr; re = expr; t = typ_opt                                         { Application (le, re, t) }
    | MATCH; expr = expr; WITH; match_cases = match_cases { Match (expr, match_cases) }
    | tb = tuple_pattern { Pattern tb }
    // | d = declaration                                                   { d }
    // | FUN; vls = nonempty_list(value); ARROW; e = expr                  { Fun (vls, e) }


pattern:
    | LEFT_PARENTHESIS; p = pattern; RIGHT_PARENTHESIS      { p }
    | const = dataType                                      { Const const } 
    | var = IDENTIFIER                               { Var var }
    | WILDCARD                                              { Wildcard }
    | lst = list_pattern                                    { List lst }  (* [1; 2; 3; ...] *)
    | lv = pattern; DOUBLE_COLON; rv = pattern              { ListConcat (lv, rv) } (* hd :: tl *)
    | LEFT_PARENTHESIS; op = bop; RIGHT_PARENTHESIS         { Operation (Binary op) } (* for prefix operations like "(+) 1 2" *)

(* default operations like "1 + 2" *)
operation_expr: 
    | e1 = expr; op = bop; e2 = expr { Application ( Application (Pattern (Operation (Binary op)), e1, None), e2, None ) }
    | op = uop; e = expr             { Application (Pattern (Operation (Unary op)), e, None) } 

if_expr:
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr                   { If (e1, e2, Some e3) }
    | IF; e1 = expr; THEN; e2 = expr                                    { If (e1, e2, None) }

// %inline func_id: 
//     // | id = IDENTIFIER     { VarId ( id ) }
//     | op = OP_IDENTIFIER  { VarId ( String.make 1 op ) }
//     // (* let _ = .. *)
//     // | WILDCARD            { Wildcard }
//     // (* let f x = let (k, j) = x in j in f (1, 2) *)
//     // | p = pattern         { p }
//     | v = value            { v }

// const_or_var: (* Const or variable *)
//     | const = dataType      { Const const} 
//     | var = IDENTIFIER    { Var var }

// %inline tuple_pattern: LEFT_PARENTHESIS; args = separated_nonempty_list(COMMA, pattern); RIGHT_PARENTHESIS { args }
%inline list_pattern: LEFT_SQ_BRACKET; args = separated_list(SEMICOLON, expr); RIGHT_SQ_BRACKET         { args }

%inline tuple_pattern:
    | t = tuple_body { t }
    | LEFT_PARENTHESIS; t = tuple_body; RIGHT_PARENTHESIS { t }

tuple_body: p1 = expr; COMMA; p2 = expr; tl = tuple_tail { Tuple (p1,p2,tl) }

tuple_tail: 
    | COMMA; p = expr; tl = tuple_tail { p::tl }
    | { [] }

%inline match_cases:
    | hd =  first_match_case; tl = list(match_case) { hd :: tl }

%inline first_match_case: bar_opt; v = pattern; ARROW; e = expr { (v, e) }

%inline bar_opt:
    | BAR { () }
    |     { () }

%inline match_case: BAR; v = pattern; ARROW; e = expr { (v, e) }

%inline rec_flag:
    | REC   { Recursive } 
    |       { Nonrecursive }

// %inline let_def:
//     (* () = print_endline "123" *)
//     | TYPE_UNIT; EQUAL; e = expr    { Let(Nonrecursive, Const(Unit), [], e)}
//     (* [rec] f x = x *)
//     | rec_opt = rec_flag; fun_name = func_id; args = list(value); EQUAL; e = expr   { Let(rec_opt, fun_name, args, e) }


// (* a = 10 and b = 20 and ... *)
// and_bind:
//     | l = let_def                             { [l] }
//     | h = let_def; LET_AND; tl = and_bind     { h :: tl }

// (* a = 10 and b = 20 and ... in a + b + ... *)
// let_and_in_def: 
//     | e1 = let_def; IN; e2 = expr   { LetAndIn ([e1], Some e2) }    (* without and *)
//     | exs = and_bind; IN; e = expr  { LetAndIn (exs, Some e) }      (* with and *)
//     | exs = and_bind;               { LetAndIn (exs, None) }        (* and .. and .. without IN *)
