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
%token INSTANCE
%token TEXT
%token NUMBER
%token FN
%token UNSAFE
%token EXCLAMATION
%token CIRCUMFLEX
%token PLUS_EQ
%token HYPHEN_EQ
%token BIG_EQ
%token COLON_EQ
%token EXTERNAL
%token INTERNAL
%token DATA
%token RAISE
%token CATCH
%token BIG_DOT
%token HYPHEN_GREATER
%token DOT

%right EQ PLUS_EQ MINU_EQ
%left COMMA
%right VERTIAL BIG_VERICAL
%right AMPERSAND BIG_AMPERSAND
%left BIG_EQ LESS GREATER EXCLAMATION_EQ
%left BIG_LESS BIG_GREATER
%left PLUS HYPHEN BIG_PLUS BIG_HYPHEN
%left ASTERISK SOIDUS PERCENT
%nonassoc MATCH
%left DOT

%start<unit> compilation_unit

%%

compilation_unit: top_level_definition_list  EOF { () }

linkage:
  INTERNAL { () }
  | EXTERNAL { () }

annotation: AT IDENTIFIER LPAREN argument_list? RPAREN { () }

annotation_list:
  annotation { () }
  | annotation annotation_list { () }

definition: annotation_list? linkage? UNSAFE? DEF pattern parameter_list? EQ expression where_clause? { () }

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
  | expression BIG_EQ expression { () }
  | expression EXCLAMATION_EQ expression { () }
  | expression AMPERSAND expression { () }
  | expression BIG_AMPERSAND expression { () }
  | expression VERTICAL expression { () }
  | expression BIG_VERTICAL expression { () }
  | expression COMMA expression { () }
  | expression MATCH LBRACKET pattern_clause_list RBRACKET { () }
  | expression PLUS_EQ expression { () }
  | expression HYPHEN_EQ expression { () }
  | expression COLON_EQ expression { () }
  | EXCLAMATION expression { () }
  | PLUS expression { () }
  | HYPHEN expression { () }
  | CIRCUMFLEX expression { () }
  | AMPERSAND expression { () }
  | ASTERISK expression { () }
  | IF LPAREN expression RPAREN expression ELSE expression { () }
  | IF LPAREN expression RPAREN expression { () }
  | expression COLON type_expression { () }
  | expression SEMI expression { () }
  | TEXT { () }
  | NUMBER { () }
  | FN parameter_list EQ_GREATER expression { () }
  | RAISE expression { () }
  | IDENTIFIER HYPHEN_GREATER expression { () }
  | expression DOT IDENTIFIER { () }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause SEMI pattern_clause_list { () }

pattern_clause:
  CASE pattern pattern_condition? EQ_GREATER expression { () }
  | CATCH TYPE_IDENTIFIER pattern pattern_condition? EQ_GREATER expression { () }

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
  | TEXT { () }
  | NUMBER { () }
  | IDENTIFIER HYPHEN_GREATER pattern { () }

argument_list:
  expression { () }
  | expression argument_list { () }

parameter_list:
  pattern { () }
  | pattern parameter_list { () }

where_clause: WHERE LBRACKET definition_list RBRACKET { () }

definition_list: definition { () }
  | definition SEMI definition_list { () }

type_expression:
  TYPE_IDENTIFIER { () }
  | LPAREN type_expression RPAREN { () }
  | TYPE_IDENTIFIER type_argument_list { () }
  | type_expression COMMA type_expression { () }
  | ASTERISK type_expression { () }
  | IDENTIFIER HYPHEN_GREATER type_expression { () }

type_argument_list:
  type_expression { () }
  | type_expression type_argument_list { () }

variant:
  DATA TYPE_IDENTIFIER variant_parameter_list? EQ variant_body { () }

variant_parameter_list:
  TYPE_IDENTIFIER { () }
  | TYPE_IDENTIFIER variant_parameter_list { () }

variant_body:
  variant_clause { () }
  | variant_clause VERTICAL variant_body { () }

variant_clause: TYPE_IDENTIFIER type_expression { () }

type_class: CLASS TYPE_IDENTIFIER type_parameter_list? EQ type_class_parameter? LBRACKET type_class_body RBRACKET { () }

type_class_parameter: TYPE_IDENTIFIER type_parameter_list? EQ_GREATER { () }

type_parameter_list:
  TYPE_IDENTIFIER { () }
  | TYPE_IDENTIFIER type_parameter_list { () }

type_class_body: declaration_list { () }

declaration_list:
  declaration { () }
  | declaration SEMI declaration_list { () }

declaration: DEF IDENTIFIER COLON type_expression { () }

instance: INSTANCE TYPE_IDENTIFIER type_argument_list EQ LBRACKET definition_list RBRACKET { () }

extensible_variant_declaration: TYPE TYPE_IDENTIFIER EQ BIG_DOT { () }

extensible_variant_definition: TYPE TYPE_IDENTIFIER PLUS_EQ variant_clause { () }

top_level_definition:
  definition { () }
  | type_class { () }
  | variant { () }
  | declaration { () }
  | extensible_variant_declaration { () }
  | extensible_variant_definition { () }

top_level_definition_list:
  top_level_definition { () }
  | top_level_definition top_level_definition_list { () }
