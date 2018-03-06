%{

%}

%token <string * Lexing.position> IDENTIFIER
%token EOF

%start <unit> compilation_unit
%%

compilation_unit: id=IDENTIFIER? EOF {
  match id with
  | Some (name, pos) ->
    Printf.printf "%s\n" name;
  | _ ->
    Printf.printf "You're empty.\n";
}