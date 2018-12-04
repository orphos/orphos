%{

%}

%token EQ
%token DEF
%token <string * Lexing.position> IDENTIFIER
%token EOF

%start<unit> value_definition

%%

value_definition: DEF IDENTIFIER EQ expression { () }

expression: IDENTIFIER { () }
