open TokenTypes

let tokenize input =
  let length = String.length input in 
  let rec helper position =
    if position >= length then [EOF]  
    else if (Str.string_match (Str.regexp "-?[0-9]+") input position) then 
    let matched_int = Str.matched_string input in 
    Tok_Int (int_of_string matched_int )::(helper (position + (String.length matched_int)))
    else if (Str.string_match (Str.regexp "-") input position) then 
    Tok_Sub:: (helper (position + 1))
    else if (Str.string_match (Str.regexp ";") input position) then 
    Tok_Semi:: (helper (position + 1))
    else if (Str.string_match (Str.regexp ")") input position) then 
    Tok_RParen:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "}") input position) then 
    Tok_RBrace:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "\\^") input position) then 
    Tok_Pow:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "\\+") input position) then 
    Tok_Add:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "||") input position) then 
    Tok_Or:: (helper (position + 2))
    else if (Str.string_match (Str.regexp "!=") input position) then 
    Tok_NotEqual:: (helper (position + 2))
    else if (Str.string_match (Str.regexp "!") input position) then 
    Tok_Not:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "\\*") input position) then 
    Tok_Mult:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "<=") input position) then 
    Tok_LessEqual:: (helper (position + 2))
    else if (Str.string_match (Str.regexp "<") input position) then 
    Tok_Less:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "(") input position) then 
    Tok_LParen:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "{") input position) then 
    Tok_LBrace:: (helper (position + 1))
    else if (Str.string_match (Str.regexp ">=") input position) then 
    Tok_GreaterEqual:: (helper (position + 2))
    else if (Str.string_match (Str.regexp ">") input position) then 
    Tok_Greater:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "==") input position) then 
    Tok_Equal:: (helper (position + 2))
    else if (Str.string_match (Str.regexp "/") input position) then 
    Tok_Div:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "=") input position) then 
    Tok_Assign:: (helper (position + 1))
    else if (Str.string_match (Str.regexp "&&") input position) then 
    Tok_And:: (helper (position + 2))
    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input position) then 
    let matched_str = Str.matched_string input in
        if matched_str = "for" then Tok_For:: (helper (position + 3))
        else if matched_str = "from" then Tok_From:: (helper (position + 4))
        else if matched_str = "to" then Tok_To:: (helper (position + 2))
        else if matched_str = "while" then Tok_While:: (helper (position + 5))
        else if matched_str = "int" then Tok_Int_Type:: (helper (position + 3))
        else if matched_str = "bool" then Tok_Bool_Type:: (helper (position + 4))
        else if matched_str = "printf" then Tok_Print:: (helper (position + 6))
        else if matched_str = "main" then Tok_Main:: (helper (position + 4)) 
        else if matched_str = "if" then Tok_If:: (helper (position + 2))
        else if matched_str = "else" then Tok_Else:: (helper (position + 4))
        else if matched_str = "true" then Tok_Bool (bool_of_string matched_str):: (helper (position + (String.length matched_str)))
        else if matched_str = "false" then Tok_Bool (bool_of_string matched_str):: (helper (position + (String.length matched_str)))
        else 
        Tok_ID (matched_str):: (helper (position + (String.length matched_str)))
  
    else helper (position + 1) in

    helper 0 ;;
  
  
