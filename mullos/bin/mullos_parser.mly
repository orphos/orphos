%{

%}

%token EQ
%token LPAREN
%token RPAREN
%token LCBRACKET
%token RCBRACKET
%token SEMI
%token COMMA
%token DEF
%token LET
%token WHERE
%token IDENTIFIER
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
%token DERIVING
%token LABEL
%token GOTO
%token BOOL
%token DOLLAR
%token NUMBERSIGN
%token THEN
%token TYPEVAR_IDENTIFIER

%right DOLLAR
%right COLON_EQ PLUS_EQ HYPHEN_EQ
%left COMMA
%right VERTICAL BIG_VERTICAL
%right AMPERSAND BIG_AMPERSAND
%left BIG_EQ LESS GREATER EXCLAMATION_EQ
%left BIG_LESS BIG_GREATER
%left PLUS HYPHEN BIG_PLUS BIG_HYPHEN
%left ASTERISK SOLIDUS PERCENT
%nonassoc MATCH
%left NUMBERSIGN

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

value_name:
  IDENTIFIER DOT value_name { () }
  | IDENTIFIER { () }

type_name:
  TYPE_IDENTIFIER DOT type_name { () }
  | TYPE_IDENTIFIER { () }
  | TYPEVAR_IDENTIFIER { () }

expression: IDENTIFIER { () }
  | LPAREN RPAREN { () }
  | LPAREN expression RPAREN { () }
  | LCBRACKET expression RCBRACKET { () }
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
  | expression MATCH LCBRACKET pattern_clause_list RCBRACKET { () }
  | expression PLUS_EQ expression { () }
  | expression HYPHEN_EQ expression { () }
  | expression COLON_EQ expression { () }
  | expression DOLLAR expression { () }
  | EXCLAMATION expression { () }
  | PLUS expression { () }
  | HYPHEN expression { () }
  | CIRCUMFLEX expression { () }
  | AMPERSAND expression { () }
  | ASTERISK expression { () }
  | IF expression THEN expression ELSE expression { () }
  | IF expression THEN expression { () }
  | expression COLON type_expression { () }
  | expression SEMI expression { () }
  | TEXT { () }
  | NUMBER { () }
  | BOOL { () }
  | FN parameter_list EQ_GREATER expression { () }
  | RAISE expression { () }
  | IDENTIFIER HYPHEN_GREATER expression { () }
  | expression NUMBERSIGN IDENTIFIER { () }
  | GOTO expression { () }
  | label_clause expression { () }

label_clause: LABEL IDENTIFIER COLON { () }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause pattern_clause_list { () }

pattern_clause:
  CASE pattern pattern_condition? EQ_GREATER expression { () }
  | CATCH type_name pattern pattern_condition? EQ_GREATER expression { () }

pattern_condition: IF expression { () }

pattern:
  value_name { () }
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
  | BOOL { () }
  | IDENTIFIER HYPHEN_GREATER pattern { () }

argument_list:
  expression { () }
  | expression argument_list { () }

parameter_list:
  pattern { () }
  | pattern parameter_list { () }

where_clause: WHERE LCBRACKET definition_list RCBRACKET { () }

definition_list: definition { () }
  | definition definition_list { () }

type_expression:
  type_name { () }
  | LPAREN type_expression RPAREN { () }
  | type_name type_argument_list { () }
  | type_expression COMMA type_expression { () }
  | ASTERISK type_expression { () }
  | IDENTIFIER HYPHEN_GREATER type_expression { () }
  | type_expression LCBRACKET effect_expression RCBRACKET { () }
  | NUMBER { () }
  | TEXT { () }
  | BOOL { () }
  | type_expression EQ_GREATER type_expression { () }

type_argument_list:
  type_expression { () }
  | type_expression type_argument_list { () }

effect_expression:
  type_expression { () }
  | type_expression BIG_PLUS effect_expression { () }
  | LOWLINE { () }

variant:
  DATA TYPE_IDENTIFIER variant_parameter_list? deriving_clause_body? EQ variant_constructor_list { () }
  | DATA TYPE_IDENTIFIER variant_parameter_list? deriving_clause_body? EQ type_expression { () }

variant_parameter_list:
  TYPEVAR_IDENTIFIER { () }
  | TYPEVAR_IDENTIFIER variant_parameter_list { () }

variant_constructor_list:
  variant_constructor { () }
  | variant_constructor VERTICAL variant_constructor_list { () }

variant_constructor:
  TYPE_IDENTIFIER variant_constructor_parameter_and_result { () }
  | TYPE_IDENTIFIER { () }

variant_constructor_parameter_and_result:
  COLON type_expression { () }
  | COLON type_expression variant_constructor_result { () }

variant_constructor_result: EQ_GREATER type_expression { () }

deriving_clause: DERIVING deriving_clause_body { () }

deriving_clause_body:
  type_name { () }
  | type_name COMMA deriving_clause_body { () }

type_class: CLASS TYPE_IDENTIFIER type_parameter_list? EQ type_class_parameter? LCBRACKET type_class_body RCBRACKET { () }

type_class_parameter: type_name type_parameter_list? EQ_GREATER { () }

type_parameter_list:
  type_name { () }
  | type_name type_parameter_list { () }

type_class_body: declaration_list { () }

declaration_list:
  declaration { () }
  | declaration declaration_list { () }

declaration: DEF IDENTIFIER COLON type_expression { () }

instance: INSTANCE TYPE_IDENTIFIER type_argument_list EQ LCBRACKET definition_list RCBRACKET { () }

extensible_variant_declaration: TYPE TYPE_IDENTIFIER EQ BIG_DOT { () }

extensible_variant_definition: TYPE TYPE_IDENTIFIER PLUS_EQ variant_constructor { () }

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
