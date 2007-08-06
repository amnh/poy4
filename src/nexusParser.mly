/* Parser for nexus files */

%token <string> ANCSTATES
%token <string> ASSUMPTIONS
%token <string> AVERAGE
%token <string> BEGIN
%token <string> BINHEX
%token <string> BOTH
%token <string> CHANGESET
%token <char> CHAR
%token <string> CHARACTER
%token <string> CHARACTERS
%token <string> CHARLABELS
%token <string> CHARPARTITION
%token <string> CHARSET
%token <string> CHARSTATELABELS
%token <string> CODEORDER
%token <string> CODESET
%token <string> CODONS
%token <string> CONDONPOSSET
%token <string> CONTINUOUS
%token <string> COUNT
%token <string> CSTREE
%token <string> DATA
%token <string> DATA_CSTREE
%token <string> QUOTED
%token <string> DATATYPE
%token <string> DEFTYPE
%token <string> DIAGONAL
%token <string> DIMENSIONS
%token <string> DISTANCES
%token <string> DNA
%token <string> DROS
%token <string> ELIMINATE
%token <string> ENCODE
%token <string> END
%token <string> EPS
%token <string> EQUATE
%token <string> EXSET
%token <string> EXT
%token <string> EXTENSIONS
%token <string> FILE
%token <string> FORMAT
%token <string> FREQUENCY
%token <string> GAP
%token <string> GAPMODE
%token <string> GENETICCODE
%token <string> GIF
%token <string> INDIVIDUALS
%token <string> INLINE
%token <string> INTERLEAVE
%token <string> ITEMS
%token <string> JPEG
%token <string> LABELS
%token <string> LOWER
%token <string> MAM
%token <string> MATCHCHAR
%token <string> MATRIX
%token <string> MAX
%token <string> MAXSTEPS
%token <string> MEDIAN
%token <string> MIN
%token <string> MINSTEPS
%token <string> MISSING
%token <string> MTDNA
%token <string> NCHAR
%token <string> NEWSTATE
%token <string> NEWTAXA
%token <string> NO
%token <string> NONE
%token <string> NOTES
%token <string> NTAX
%token <string> NUCLEOTIDE
%token <string> NUCORDER
%token <string> OPTIONS
%token <string> PICT
%token <string> PICTURE
%token <string> POLYTCOUNT
%token <string> PROTEIN
%token <string> RESOURCE
%token <string> RESPECTCASE
%token <string> RNA
%token <string> SAMPLESIZE
%token <string> SETS
%token <string> SOURCE
%token <string> STANDARD
%token <string> STATE
%token <string> STATELABELS
%token <string> STATES
%token <string> STATESET
%token <string> STATESFORMAT
%token <string> STATESPRESENT
%token <string> STDERROR
%token <string> STEPMATRIX
%token <string> SYMBOLS
%token <string> TAXON
%token <string> TAXA
%token <string> TAXLABELS
%token <string> TAXPARTITION
%token <string> TAXSET
%token <string> TEXT
%token <string> TIFF
%token <string> TOKENS
%token <string> TRANSLATE
%token <string> TRANSPOSE
%token <string> TREE
%token <string> TREEPARTITION
%token <string> TREES
%token <string> TREESET
%token <string> TRIANGLE
%token <string> TYPESET
%token <string> UNALIGNED
%token <string> UNIVERSAL
%token <string> UPPER
%token <string> USERTYPE
%token <string> UUENCODE
%token <string> VARIANCE
%token <string> VECTOR
%token <string> WTSET
%token <string> YEAST
%token <string> EIDENT
%token NEXUS SEMICOLON EQUAL COMMA QUOTE BACKSLASH DASH LPARENT RPARENT STAR
COLON
%token <string> IDENT
%token <string> FLOAT
%token <string> INTEGER
%start header
%type <unit> header
%start block
%type <Nexus.block> block
%start symbol_pair
%type <(string * string list)> symbol_pair
%start symbol_list
%type <string list> symbol_list
%%

header:
    | NEXUS { () }
    ;
block:
    | BEGIN TAXA SEMICOLON taxa END SEMICOLON  
        { `Taxa $4 }
    | BEGIN CHARACTERS SEMICOLON characters END SEMICOLON 
        { `Characters $4 }
    | BEGIN DATA SEMICOLON characters END SEMICOLON
        { `Characters $4 }
    | BEGIN UNALIGNED SEMICOLON unaligned END SEMICOLON
        { `Unaligned $4 }
    | BEGIN TREES SEMICOLON trees END SEMICOLON
        { `Trees $4 }
    | BEGIN NOTES SEMICOLON notes END SEMICOLON 
        { `Notes $4 }
    | BEGIN DISTANCES SEMICOLON distances END SEMICOLON 
        { `Distances $4 }
    | BEGIN ASSUMPTIONS SEMICOLON assumptions END SEMICOLON
        { `Assumptions $4 }
    | BEGIN SETS SEMICOLON list_of_anything END SEMICOLON
        { `Ignore $2 }
    | BEGIN IDENT list_of_anything END SEMICOLON
        { `Ignore $2 }
    ;
assumptions:
    | optional_assumption_options optional_user_type optional_type_set 
     optional_wtset optional_exset optional_ancstates
        { ($1, $2, $3, $4, $5, $6) }
    ;
optional_assumption_options:
    | OPTIONS deftype polytcount gapmode SEMICOLON
        { ($2, $3, $4) }
    |   { None, `MinSteps, `NewState }
    ;
deftype:
    | DEFTYPE EQUAL IDENT { Some $3 }
    | { None }
    ;
polytcount:
    | POLYTCOUNT EQUAL MINSTEPS { `MinSteps }
    | POLYTCOUNT EQUAL MAXSTEPS { `MaxSteps }
    | { `MinSteps }
    ;
gapmode:
    | GAPMODE EQUAL MISSING { `Missing }
    | GAPMODE EQUAL NEWSTATE { `NewState }
    | { `NewState }
    ;
optional_user_type:
    | USERTYPE IDENT user_type_definition SEMICOLON { Some ($2, $3) }
    | { None }
    ;
user_type_definition:
    | EQUAL INTEGER numbers_and_chars { `StepMatrix ($2, $3) }
    | LPARENT STEPMATRIX RPARENT EQUAL INTEGER numbers_and_chars 
        { `StepMatrix ($5, $6) }
    | LPARENT CSTREE DATA_CSTREE { `CSTree $3 }
    ;
numbers_and_chars:
    | number_and_char numbers_and_chars { $1 :: $2 }
    | number_and_char { [$1] }
    ;
number_and_char:
    | INTEGER   { $1 }
    | FLOAT     { $1 }
    | CHAR      { Char.escaped $1 }
    ;
optional_type_set:
    | TYPESET optional_set_for_assumptions { $2 }
    | { None }
    ;
optional_wtset:
    | WTSET optional_set_for_assumptions { $2 }
    | { None }
    ;
optional_set_for_assumptions:
    | IDENT NO TOKENS EQUAL standard_type_set SEMICOLON 
        { Some (false, $1, false, `Standard $5) }
    | STAR IDENT NO TOKENS EQUAL standard_type_set SEMICOLON
        { Some (true, $2, false, `Standard $6) }
    | IDENT TOKENS EQUAL standard_type_set SEMICOLON
        { Some (false, $1, true, `Standard $4) }
    | STAR IDENT TOKENS EQUAL standard_type_set SEMICOLON
        { Some (true, $2, true, `Standard $5) }
    | IDENT LPARENT STANDARD RPARENT NO TOKENS EQUAL standard_type_set SEMICOLON 
        { Some (false, $1, false, `Standard $8) }
    | STAR IDENT LPARENT STANDARD RPARENT NO TOKENS EQUAL standard_type_set SEMICOLON
        { Some (true, $2, false, `Standard $9) }
    | IDENT LPARENT STANDARD RPARENT TOKENS EQUAL standard_type_set SEMICOLON
        { Some (false, $1, true, `Standard $7) }
    | STAR IDENT LPARENT STANDARD RPARENT TOKENS EQUAL standard_type_set SEMICOLON
        { Some (true, $2, true, `Standard $8) }
    | IDENT LPARENT VECTOR RPARENT NO TOKENS EQUAL vector_type_set SEMICOLON 
        { Some (false, $1, false,`Vector $8) }
    | STAR IDENT LPARENT VECTOR RPARENT NO TOKENS EQUAL vector_type_set SEMICOLON
        { Some (true, $2, false, `Vector $9) }
    | IDENT LPARENT VECTOR RPARENT TOKENS EQUAL vector_type_set SEMICOLON
        { Some (false, $1, true, `Vector $7) }
    | STAR IDENT LPARENT VECTOR RPARENT TOKENS EQUAL vector_type_set SEMICOLON
        { Some (true, $2, true, `Vector $8) }
    ;
standard_type_set_item:
    | INTEGER COLON characterset_list { `Code ($1, $3) }
    | IDENT COLON characterset_list   { `Name ($1, $3) }
    ;
standard_type_set:
    | standard_type_set_item COMMA standard_type_set { ($1 :: $3) }
    | standard_type_set_item { [$1] }
    ;
vector_type_set:
    | INTEGER vector_type_set { ($1 :: $2) }
    | INTEGER { [$1] }
    ;
optional_exset:
    | EXSET optional_set_for_assumptions { $2 }
    | { None }
    ;
optional_ancstates:
    | ANCSTATES optional_set_for_assumptions { $2 }
    | { None }
    ;
distances:
    | optional_distances_dimensions optional_format optional_taxlabels DATA SEMICOLON
        { ($1, $2, $3, $4) }
    ;
optional_distances_dimensions:
    | DIMENSIONS NEWTAXA NTAX EQUAL INTEGER NCHAR EQUAL INTEGER SEMICOLON
        { Some (true, $5, $8) }
    | DIMENSIONS NTAX EQUAL INTEGER NCHAR EQUAL INTEGER SEMICOLON
        { Some (false, $4, $7) }
    | { None }
    ;
notes:
    | optional_text optional_picture 
        { $1, $2 }
    ;
optional_text:
    | TEXT optional_set_pair_list SOURCE EQUAL source DATA SEMICOLON
        { Some ($2, $5, $6) }
    | { None }
    ;
optional_picture:
    | PICTURE optional_set_pair_list optional_pictureformat optional_encode SOURCE EQUAL source 
    DATA SEMICOLON { Some ($2, $3, $4, $7, $8) }
    | { None } 
    ;
optional_set_pair_list:
    | set_pair optional_set_pair_list { $1 :: $2 }
    | { [] }
    ;
set_pair:
    | TAXON EQUAL characterset     { `TaxonSet $3 }
    | CHARACTER EQUAL characterset { `CharacterSet $3 }
    | STATE EQUAL characterset      { `StateSet $3 }
    | TREE EQUAL characterset        { `TreeSet $3 }
    ;
source:
    | INLINE    {`Inline }
    | FILE      { `File }
    | RESOURCE  { `Resource }
    ;
optional_pictureformat:
    | FORMAT EQUAL pictureformat { Some $3 }
    | { None }
    ;
pictureformat:
    | PICT      { `Pict }
    | TIFF      { `Tiff }
    | EPS       { `Eps }
    | JPEG      { `Jpeg }
    | GIF       { `Gif }
    ;
optional_encode:
    | ENCODE EQUAL pictureencoding { Some $3 }
    | { None }
    ;
pictureencoding:
    | NONE  { `None }
    | UUENCODE { `UUEncode }
    | BINHEX    { `BinHex }
    ;
trees:
    | optional_translate tree_list SEMICOLON { ($1, $2) }
    ;
optional_translate:
    | TRANSLATE pairs_list SEMICOLON { $2 }
    | { [] }
    ;
tree_list:
    | DATA tree_list { $1 :: $2 }
    | DATA           { [ $1 ] }
    ;
pairs_list:
    | IDENT IDENT COMMA pairs_list { ($1, $2) :: $4 }
    | IDENT IDENT { [$1, $2] }
    ;
characters:
    | DIMENSIONS optional_taxa_dimensions NCHAR EQUAL INTEGER SEMICOLON 
     optional_format optional_eliminate optional_taxlabels
     optional_charstatelabels optional_charlabels optional_statelabels 
     DATA SEMICOLON 
     { 
         { Nexus.char_taxon_dimensions = $2;
         Nexus.char_char_dimensions = $5;
         Nexus.char_format = $7;
         Nexus.char_eliminate = $8;
         Nexus.char_taxlabels = $9;
         Nexus.char_statelabels = $10;
         Nexus.char_charlabels = $11;
         Nexus.char_charstates = $12;
         Nexus.chars = $13;}
     }
unaligned:
    | optional_unaligned_dimensions optional_format DATA SEMICOLON
        { { Nexus.unal_taxon_dimensions = $1; unal_format = $2; unal = $3 } }
     ;
optional_unaligned_dimensions:
    | DIMENSIONS optional_taxa_dimensions { $2 }
    | { None }
    ;
optional_charlabels:
    | CHARLABELS taxonlist SEMICOLON    { $2 }
    |                                   { [] }
    ;
optional_charstatelabels:
    | CHARSTATELABELS charstatelables SEMICOLON { $2 }
    |                                           { [] }
    ;
charstatelables:
    | INTEGER IDENT BACKSLASH taxonlist COMMA charstatelables { ($1, $2, $4) :: $6 }
    | INTEGER IDENT BACKSLASH taxonlist { [] }
    ;
optional_statelabels:
    | STATELABELS statelabels SEMICOLON { $2 }
    |                           { [] }
    ;
statelabels:
    | INTEGER taxonlist COMMA statelabels { ($1, $2) :: $4 }
    |                                       { [] }
    ;
optional_taxlabels:
    | TAXLABELS taxonlist SEMICOLON { $2 }
    |                           { [] }
    ;
optional_eliminate:
    | ELIMINATE characterset SEMICOLON { Some $2 }
    |                           { None }
    ;
optional_format:
    | FORMAT format_items_list SEMICOLON { $2 }
    |           { [] }
    ;
format_items_list:
    | format_items format_items_list { $1 :: $2 }
    |           { [] }
    ;
format_items:
    | DATATYPE EQUAL datatype { `Datatype $3 }
    | RESPECTCASE { `RespectCase }
    | MISSING EQUAL symbol { `Missing $3 }
    | GAP EQUAL symbol { `Gap $3 }
    | SYMBOLS EQUAL QUOTED { 
        let len = String.length $3 in
        `Symbols (String.sub $3 0 (len - 1))}
    | EQUATE EQUAL QUOTED { `Equate $3 }
    | MATCHCHAR EQUAL symbol { `MatchChar $3 }
    | NO LABELS     { `Labels false }
    | LABELS        { `Labels true }
    | TRANSPOSE     { `Transpose }
    | INTERLEAVE    { `Interleave }
    | ITEMS EQUAL item { `Items $3 }
    | STATESFORMAT EQUAL states_format { `StatesFormat $3 }
    | NO TOKENS     { `Tokens false }
    | TOKENS        { `Tokens true }
    | TRIANGLE EQUAL triangle_format { `Triangle $3 }
    ;
triangle_format:
    | LOWER { `Lower }
    | UPPER { `Upper }
    | BOTH { `Both }
    ;
datatype:
    | STANDARD { `Standard }
    | DNA { `Dna }
    | RNA { `Rna }
    | NUCLEOTIDE { `Nucleotide }
    | PROTEIN { `Protein }
    | CONTINUOUS { `Continuous }
    ;
symbol:
    | IDENT { $1 }
    | DASH { "-" }
    | CHAR  { Char.escaped $1 }
    ;
symbol_list:
    | symbol symbol_list { $1 :: $2 }
    |       { [] }
    ;
symbol_pair:
    | symbol EQUAL symbol {($1, [$3])}
    | symbol EQUAL LPARENT symbol_list RPARENT { ($1, $4) }
    ;
item:
    | MIN { `Min }
    | MAX { `Max }
    | MEDIAN  { `Median }
    | AVERAGE { `Average }
    | VARIANCE { `Variance }
    | STDERROR { `Stderror }
    | SAMPLESIZE { `SampleSize }
    | STATES     { `States }
    ;
states_format:
    | STATESPRESENT { `StatesPresent }
    | INDIVIDUALS   { `Individuals }
    | COUNT         { `Count }
    | FREQUENCY     { `Frequency }
    ;
optional_taxa_dimensions:
    | NTAX EQUAL INTEGER { Some $3 }
    |               { None }
    ;
taxa:
    | DIMENSIONS NTAX EQUAL INTEGER SEMICOLON TAXLABELS taxonlist SEMICOLON 
        { ($4, $7) }
    ;
taxonlist:
    | IDENT taxonlist   { $1 :: $2 }
    |                   { [] }
    ;
characterset_list:
    | characterset characterset_list { $1 :: $2 }
    | characterset      { [$1] }
    ;
characterset:
    | INTEGER DASH INTEGER { `Range ($1, $3) }
    | INTEGER              { `Single ($1) }
    | IDENT                { `Name $1 }
    ;
list_of_anything:
    | any_thing_minus_end list_of_anything { () }
    | any_thing_minus_end { () }
    ;
any_thing_minus_end:
    | ANCSTATES { () }
    | AVERAGE { () }
    | BINHEX { () }
    | BOTH { () }
    | CHANGESET { () }
    | CHAR { () }
    | CHARACTER { () }
    | CHARLABELS { () }
    | CHARPARTITION { () }
    | CHARSET { () }
    | CHARSTATELABELS { () }
    | CODEORDER { () }
    | CODESET { () }
    | CODONS { () }
    | CONDONPOSSET { () }
    | CONTINUOUS { () }
    | COUNT { () }
    | CSTREE { () }
    | DATA_CSTREE { () }
    | QUOTED { () }
    | DATATYPE { () }
    | DEFTYPE { () }
    | DIAGONAL { () }
    | DIMENSIONS { () }
    | DNA { () }
    | DROS { () }
    | ELIMINATE { () }
    | ENCODE { () }
    | EPS { () }
    | EQUATE { () }
    | EXSET { () }
    | EXT { () }
    | EXTENSIONS { () }
    | FILE { () }
    | FORMAT { () }
    | FREQUENCY { () }
    | GAP { () }
    | GAPMODE { () }
    | GENETICCODE { () }
    | GIF { () }
    | INDIVIDUALS { () }
    | INLINE { () }
    | INTERLEAVE { () }
    | ITEMS { () }
    | JPEG { () }
    | LABELS { () }
    | LOWER { () }
    | MAM { () }
    | MATCHCHAR { () }
    | MATRIX { () }
    | MAX { () }
    | MAXSTEPS { () }
    | MEDIAN { () }
    | MIN { () }
    | MINSTEPS { () }
    | MISSING { () }
    | MTDNA { () }
    | NCHAR { () }
    | NEWSTATE { () }
    | NEWTAXA { () }
    | NO { () }
    | NONE { () }
    | NTAX { () }
    | NUCLEOTIDE { () }
    | NUCORDER { () }
    | OPTIONS { () }
    | PICT { () }
    | PICTURE { () }
    | POLYTCOUNT { () }
    | PROTEIN { () }
    | RESOURCE { () }
    | RESPECTCASE { () }
    | RNA { () }
    | SAMPLESIZE { () }
    | SETS { () }
    | SOURCE { () }
    | STANDARD { () }
    | STATE { () }
    | STATELABELS { () }
    | STATES { () }
    | STATESET { () }
    | STATESFORMAT { () }
    | STATESPRESENT { () }
    | STDERROR { () }
    | STEPMATRIX { () }
    | SYMBOLS { () }
    | TAXON { () }
    | TAXLABELS { () }
    | TAXPARTITION { () }
    | TAXSET { () }
    | TEXT { () }
    | TIFF { () }
    | TOKENS { () }
    | TRANSLATE { () }
    | TRANSPOSE { () }
    | TREE { () }
    | TREEPARTITION { () }
    | TREESET { () }
    | TRIANGLE { () }
    | TYPESET { () }
    | UNIVERSAL { () }
    | UPPER { () }
    | USERTYPE { () }
    | UUENCODE { () }
    | VARIANCE { () }
    | VECTOR { () }
    | WTSET { () }
    | YEAST { () }
    | EIDENT { () }
    | STAR { () }
    | COLON { () }
    | IDENT { () }
    | FLOAT { () }
    | INTEGER { () }
    | SEMICOLON  { () }
    | EQUAL { () }
    | COMMA { () }
    | QUOTE { () }
    | BACKSLASH { () }
    | DASH { () }
    | LPARENT { () }
    | RPARENT { () }
    ;
