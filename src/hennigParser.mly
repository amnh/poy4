/* Parser for hennig files */
%token <string> DATA TREES
%token <string> TREAD
%token <string> WORD
%token <string> INT
%token <string> CHARNAME
%token <char> CHAR
%token CCODE COST PROCESS OPTCODE CHARNAMECMD
%token LPARENT RPARENT GT EQUAL QUESTION SEMICOLON DASH LSQ RSQ PLUS STAR BACKSLASH LBRACKET RBRACKET DOT 
%type <Hennig.command> command
%start command
%type <(int * int * string)> xread
%start xread
%%

command:
    | DATA SEMICOLON { Hennig.Xread $1 }
    | CCODE character_change_list SEMICOLON { Hennig.Ccode $2 }
    | TREES SEMICOLON { Hennig.Tread $1 }
    | COST cost_change_list SEMICOLON { Hennig.Cost $2 }
    | PROCESS BACKSLASH SEMICOLON { Hennig.Ignore }
    | OPTCODE INT DOT INT SEMICOLON { Hennig.Ignore }
    | CHARNAMECMD char_names_list SEMICOLON { Hennig.Charname $2 }
    | anything_list SEMICOLON { Hennig.Ignore }
anything_list:
    | anything anything_list { [] }
    |    { [] }
anything:
    | WORD { [] }
    | INT { [] }
    | CHAR { [] }
    | LPARENT { []}
    | RPARENT { [] }
    | GT { [] }
    | EQUAL {[]}
    | QUESTION { [] }
    | DASH { [] }
    | LSQ { [] }
    | PLUS { [] }
    | RSQ  { [] }
    | STAR  { [] }
    | BACKSLASH { [] }
    | LBRACKET { [] }
    | RBRACKET { [] }
    | DOT { [] }
char_names_list:
    | CHARNAME char_names_list { $1 :: $2 }
    | { [] }
character_change_list:
    | character_change character_change_list { $1 :: $2 }
    | { [] }
character_change:
    | PLUS character_list   { Hennig.Additive $2 }
    | DASH character_list   { Hennig.NonAdditive $2 }
    | LSQ character_list    { Hennig.Active $2 }
    | RSQ character_list    { Hennig.Inactive $2 }
    | LPARENT character_list { Hennig.Sankoff $2 }
    | RPARENT character_list { Hennig.NonAdditive $2 }
    | BACKSLASH INT character_list { Hennig.Weight (int_of_string $2, $3) }
character_list:
    | INT DOT INT { [Hennig.Range (int_of_string $1, int_of_string $3)] }
    | INT { [Hennig.Single (int_of_string $1)] }
    | DOT { [Hennig.All] }
cost_change_list:
    | cost_change cost_change_list { $1 :: $2 }
    | { [] }
cost_change:
    | INT EQUAL chars GT chars INT 
        { (false, int_of_string $1, ($3, $5) , int_of_string $6) }
    | INT EQUAL chars BACKSLASH chars INT
        { (true, int_of_string $1, ($3, $5) , int_of_string $6) }
chars:
    | INT { $1 }
    | LSQ int_list RSQ { String.concat "" $2 }
int_list:
    | INT int_list { $1 :: $2 }
    | { [] }
xread:
    | INT INT DATA { (int_of_string $1), (int_of_string $2), $3 }
