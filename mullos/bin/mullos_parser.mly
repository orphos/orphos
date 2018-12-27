%{

%}

%token EQ
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token SEMI
%token COMMA
%token DEF
%token LET
%token WHERE
%token <string * Lexing.position> IDENTIFIER
%token EOF
%token INDENT
%token DEDENT
%token OP_ADDPREC
%token OP_MULPREC

%left OP_ADDPREC
%left OP_MULPREC

%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

value_definition: DEF IDENTIFIER EQ expression { () }

expression: IDENTIFIER { () }
  | expression LPAREN argument_list RPAREN { () }
  | LET IDENTIFIER EQ expression SEMI expression { () }
  | expression OP_ADDPREC expression { () }
  | expression OP_MULPREC expression { () }

argument_list:
  expression { () }
  | expression COMMA argument_list { () }

where_clause: WHERE INDENT definition_list DEDENT { () }

definition_list: value_definition { () }
  | value_definition SEMI definition_list { () }
