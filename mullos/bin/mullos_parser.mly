%{

%}

%token EQ
%token LBRACKET
%token RBRACKET
%token SEMI
%token DEF
%token WHERE
%token <string * Lexing.position> IDENTIFIER
%token EOF

%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

value_definition: DEF IDENTIFIER EQ expression { () }

expression: IDENTIFIER { () }

where_clause: WHERE LBRACKET definition_list RBRACKET { () }

definition_list: value_definition { () }
  | value_definition SEMI definition_list { () }
