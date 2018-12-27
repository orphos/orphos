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
%token PLUS
%token HYPHEN
%token ASTERISK
%token SOLIDUS
%token PERCENT
%token BIG_PLUS
%token BIG_HYPHEN
%token LESS
%token GREATER
%token EXCLAMATION_EQ
%token AMPERSAND
%token BIG_AMPERSAND
%token VERTICAL
%token BIG_VERTICAL
%token BIG_LESS
%token BIG_GREATER

%right VERTIAL BIG_VERICAL
%right AMPERSAND BIG_AMPERSAND
%left EQ LESS GREATER EXCLAMATION_EQ
%left BIG_LESS BIG_GREATER
%left PLUS HYPHEN BIG_PLUS BIG_HYPHEN
%left ASTERISK SOIDUS PERCENT

%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

value_definition: DEF IDENTIFIER EQ expression where_clause? { () }

expression: IDENTIFIER { () }
  | LPAREN expression RPAREN
  | expression LPAREN argument_list RPAREN { () }
  | LET IDENTIFIER EQ expression SEMI expression { () }
  | expression PLUS expression { () }
  | expression HYPHEN expression { () }
  | expression ASTERISK expression { () }
  | expression SOLIDUS expression { () }
  | expression PERCENT expression { () }
  | expression BIG_PLUS expression { () }
  | expression BIG_HYPHEN expression { () }
  | expression BIG_LESS expression { () }
  | expression BIG_GREATER expression { () }
  | expression LESS expression { () }
  | expression GREATER expression { () }
  | expression EXCLAMATION_EQ expression { () }
  | expression AMPERSAND expression { () }
  | expression BIG_AMPERSAND expression { () }
  | expression VERTICAL expression { () }
  | expression BIG_VERTICAL expression { () }

argument_list:
  expression { () }
  | expression COMMA argument_list { () }

where_clause: WHERE INDENT definition_list DEDENT { () }

definition_list: value_definition { () }
  | value_definition SEMI definition_list { () }
