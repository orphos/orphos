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
%token ELSE
%token TYPE
%token CLASS

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

definition: DEF pattern parameter_list? EQ expression where_clause? { () }

expression: IDENTIFIER { () }
  | LPAREN RPAREN { () }
  | LPAREN expression RPAREN { () }
  | expression LPAREN argument_list RPAREN { () }
  | LET pattern parameter_list? EQ expression SEMI expression { () }
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
  | IF LPAREN expression RPAREN expression ELSE expression { () }
  | expression COLON type_expression { () }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause SEMI pattern_clause_list { () }

pattern_clause: CASE pattern pattern_condition? EQ_GREATER expression { () }

pattern_condition: IF expression { () }

pattern:
  IDENTIFIER { () }
  | LPAREN RPAREN { () }
  | LPAREN pattern RPAREN { () }
  | IDENTIFIER AT pattern { () }
  | IDENTIFIER LPAREN pattern RPAREN { () }
  | pattern COMMA pattern { () }
  | pattern BIG_COLON pattern { () }
  | pattern COLON type_expression { () }
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

type_expression:
  TYPE_IDENTIFIER { () }
  | LPAREN type_expression RPAREN { () }
  | TYPE_IDENTIFIER type_argument_list { () }
  | type_expression COMMA type_expression { () }

type_argument_list:
  type_expression { () }
  | type_expression type_argument_list { () }

variant:
  TYPE TYPE_IDENTIFIER EQ variant_body { () }

variant_body:
  variant_clause { () }
  | variant_clause VERTICAL variant_body { () }

variant_clause: TYPE_IDENTIFIER type_expression { () }

type_class: TYPE TYPE_IDENTIFIER type_parameter_list? EQ CLASS type_class_body { () }

type_parameter_list:
  TYPE_IDENTIFIER { () }
  | TYPE_IDENTIFIER type_parameter_list { () }

type_class_body: definition_list { () }
