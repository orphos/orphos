%{

%}

%token EQ
%token DEF
%token <string * Lexing.position> IDENTIFIER
%token EOF

%start<unit> compilation_unit

%%

compilation_unit: defs=value_definition*  EOF { () }

value_definition: DEF IDENTIFIER EQ expression { () }

expression: IDENTIFIER { () }
