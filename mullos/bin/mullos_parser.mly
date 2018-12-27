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
%token AT
%token COLON
%token BIG_COLON
%token TYPE_IDENTIFIER
%token MATCH
%token CASE
%token LOWLINE
%token EQ_GREATER
%token IF

%left COMMA
%right VERTIAL BIG_VERICAL
%right AMPERSAND BIG_AMPERSAND
%left EQ LESS GREATER EXCLAMATION_EQ
%left BIG_LESS BIG_GREATER
%left PLUS HYPHEN BIG_PLUS BIG_HYPHEN
%left ASTERISK SOIDUS PERCENT
%nonassoc MATCH

%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

definition: DEF pattern parameter_list EQ expression where_clause? { () }

expression: IDENTIFIER { () }
  | LPAREN expression RPAREN
  | expression LPAREN argument_list RPAREN { () }
  | LET pattern parameter_list EQ expression SEMI expression { () }
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
  | expression COMMA expression { () }
  | expression MATCH INDENT pattern_clause_list DEDENT { () }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause SEMI pattern_clause_list { () }

pattern_clause: CASE pattern pattern_condition? EQ_GREATER expression { () }

pattern_condition: IF expression { () }

pattern:
  IDENTIFIER { () }
  | LPAREN pattern RPAREN { () }
  | IDENTIFIER AT pattern { () }
  | IDENTIFIER LPAREN pattern RPAREN { () }
  | pattern COMMA pattern { () }
  | pattern BIG_COLON pattern { () }
  | pattern COLON TYPE_IDENTIFIER { () }
  | LOWLINE { () }

argument_list:
  expression { () }
  | expression argument_list { () }

parameter_list:
  pattern { () }
  | pattern parameter_list { () }

where_clause: WHERE INDENT definition_list DEDENT { () }

definition_list: definition { () }
  | definition SEMI definition_list { () }
