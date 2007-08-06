(* Hennig lexer *)
{
    open HennigParser
    exception Eof

    let ( --> ) a b = b a

    let keyword_table = Hashtbl.create 53

    let token_table = 
        let make_all_prefixes min_len fullcommand output acc =
            let max_len = String.length fullcommand in 
            let rec prepend len acc =
                if len > max_len then acc
                else 
                    prepend (len + 1) (((String.sub fullcommand 0 len), output) :: acc)
            in
            prepend min_len acc
        in
        [] 
        --> make_all_prefixes 2 "ccode" CCODE
        --> make_all_prefixes 2 "costs" COST
        --> make_all_prefixes 2 "proc" PROCESS
        --> make_all_prefixes 2 "optcode" OPTCODE
        --> make_all_prefixes 2 "cnames"    CHARNAMECMD

    let _ = 
        List.iter (fun (keyw, tok) -> Hashtbl.add keyword_table keyw tok) 
        token_table

}

rule token = parse
      [ ' ' '\t' '\n' '\010' '\013' '\012' ]    { token lexbuf }
    | "xread" { raw lexbuf }
    | "xrea"  { raw lexbuf }
    | "xre"   { raw lexbuf }
    | "xr"    { raw lexbuf }
    | "tread" { rawtree lexbuf }
    | "trea"  { rawtree lexbuf }
    | "tre"   { rawtree lexbuf }
    | "tr"    { rawtree lexbuf }
    | "ccode" { CCODE }
    | "ccod" { CCODE }
    | "cco" { CCODE }
    | "cc" { CCODE }
    | "proc"  { PROCESS }
    | "pro"  { PROCESS }
    | "pr"  { PROCESS }
    | "costs" { COST }
    | "cost" { COST }
    | "cos" { COST }
    | "co" { COST }
    | "cnames" { CHARNAMECMD }
    | "cname" { CHARNAMECMD }
    | "cnam" { CHARNAMECMD }
    | "cna" { CHARNAMECMD }
    | "cn" { CHARNAMECMD }
    | [';'] { SEMICOLON }
    | [ '-' ] { DASH }
    | [ '0' - '9']+ as id { INT id }
    | [ '[' ] { LSQ }
    | [ ']' ] { RSQ }
    | [ '{' ] { characters_names lexbuf }
    | [ '+' ] { PLUS }
    | [ '*' ] { STAR }
    | [ '/' ] { BACKSLASH }
    | [ '.' ] { DOT }
    | [ '>' ] { GT }
    | [ '=' ] { EQUAL }
    | [ '?' ] { QUESTION }
    | [ '(' ] { LPARENT }
    | [ ')' ] { RPARENT }
    | [ 'a'-'z' 'A'-'Z'] [^ ' ' '\t' '\n' '\010' '\013' '\012' ';']+ as word 
        { WORD word }
    | [ ^ '\000' ] as ch { CHAR ch } 
    | eof       { raise Eof }
and characters_names = parse
     [^ ';']+[';'] as r { CHARNAME r }
and raw = parse
     [^ ';']* as d      { DATA d }
and rawtree = parse
     [^ ';']* as d      { TREES d }
and ignore_quote = parse
    | [^ '\'']+['\'']   { xread lexbuf }
and xread = parse 
      [ ' ' '\t' '\n' '\010' '\013' '\012' ]    { xread lexbuf }
    | [ '\'' ]  { ignore_quote lexbuf }
    | [ '0'-'9']+ as integer { INT integer }
    | ['a'-'z' 'A'-'Z' '_' '-' '.' '?'][^ '\000']+ eof as data    { DATA data }
