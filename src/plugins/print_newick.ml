let lexer = Genlex.make_lexer [":"; "("; ")"; ","] 

let rec process_root = parser
    | [< 'Genlex.Kwd "("; x = tree_item; t = tree_list; 'Genlex.Kwd ")" >] ->
            Parser.Tree.Node (x :: t, "")
    | [< 'Genlex.Ident name >] ->  
            Parser.Tree.Leaf name
and tree_item = parser
    | [< 'Genlex.Kwd "("; x = tree_item; t = tree_list; 'Genlex.Kwd ")"; 
        flt = branch_length >] -> 
            Parser.Tree.Node (x :: t, flt)
    | [< 'Genlex.Ident name; _ = branch_length >] ->
            Parser.Tree.Leaf name
and tree_list = parser
    | [< 'Genlex.Kwd ","; x = tree_item; y = tree_list >] -> 
            x :: y 
    | [< >] -> 
            []
and branch_length = parser
    | [< 'Genlex.Kwd ":"; 'Genlex.Float flt >] -> string_of_float flt
    | [< >] -> ""

let process_file filename outputfile =
    let ch = open_in filename in
    let str = lexer (Stream.of_channel ch) in
    let tree = process_root str in
    let () = close_in ch in
    GraphicsPs.display "" outputfile [|0.0, tree|]

let print_tree arguments run =
    match arguments with
    | `List [`String inputfile; `String outputfile] ->
            process_file inputfile outputfile;
            run
    | _ -> failwith "Usage: print_newick (STRING, STRING), where the arguments are the input and output filenames."

let () = Phylo.register_function "print_newick" print_tree
