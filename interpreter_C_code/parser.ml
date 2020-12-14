open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) && lookahead remaining_tok <> Tok_RParenlookahead remaining_tok <> Tok_Semi &&*)

let rec parse_expr (toks : token list) : expr_result =
  let (remaining_tok, expr) = parse_OrExpr toks in 
  (remaining_tok, expr)

and parse_OrExpr (toks : token list)  =
  let (toks_after_And, expr) = parse_And toks in 
  match ( lookahead toks_after_And ) with
  | Tok_Or -> let toks2 = match_token toks_after_And Tok_Or in
                let (toks3, expr_after_Or) = parse_OrExpr toks2 in
                (toks3, Or (expr, expr_after_Or))
  |_ -> (toks_after_And, expr)  
  
and parse_And (toks : token list)  =
  let (toks_after_Eq, expr) = parse_Eq toks in 
  match ( lookahead toks_after_Eq ) with
  | Tok_And -> let toks2 = match_token toks_after_Eq Tok_And in
                let (toks3, expr_after_And) = parse_And toks2 in
                (toks3, And (expr, expr_after_And))
  |_ -> (toks_after_Eq, expr) 
  
and parse_Eq (toks : token list)  =
  let (toks_after_Rel, expr) = parse_Rel toks in 
  match ( lookahead toks_after_Rel ) with
  | Tok_Equal -> let toks2 = match_token toks_after_Rel Tok_Equal in
                let (toks3, expr_after_Eq) = parse_Eq toks2 in
                (toks3, Equal (expr, expr_after_Eq))
  | Tok_NotEqual -> let toks2 = match_token toks_after_Rel Tok_NotEqual in
                let (toks3, expr_after_Eq) = parse_Eq toks2 in
                (toks3, NotEqual (expr, expr_after_Eq))              
  |_ -> (toks_after_Rel, expr) 
  
and parse_Rel (toks : token list)  =
  let (toks_after_Add, expr) = parse_Add toks in 
  match ( lookahead toks_after_Add ) with
  | Tok_LessEqual -> let toks2 = match_token toks_after_Add Tok_LessEqual in
                let (toks3, expr_after_Rel) = parse_Rel toks2 in
                (toks3, LessEqual (expr, expr_after_Rel))
  |Tok_GreaterEqual -> let toks2 = match_token toks_after_Add Tok_GreaterEqual in
                let (toks3, expr_after_Rel) = parse_Rel toks2 in
                 (toks3, GreaterEqual (expr, expr_after_Rel))
  |Tok_Less -> let toks2 = match_token toks_after_Add Tok_Less in
                let (toks3, expr_after_Rel) = parse_Rel toks2 in
                (toks3, Less (expr, expr_after_Rel))
  |Tok_Greater -> let toks2 = match_token toks_after_Add Tok_Greater in
                let (toks3, expr_after_Rel) = parse_Rel toks2 in
                (toks3, Greater (expr, expr_after_Rel))              
  |_ -> (toks_after_Add, expr)

and parse_Add (toks : token list)  =
  let (toks_after_Mul, expr) = parse_Mul toks in 
  match ( lookahead toks_after_Mul ) with
  | Tok_Add -> let toks2 = match_token toks_after_Mul Tok_Add in
                let (toks3, expr_after_Add) = parse_Add toks2 in
                (toks3, Add (expr, expr_after_Add))
  |Tok_Sub -> let toks2 = match_token toks_after_Mul Tok_Sub in
                let (toks3, expr_after_Add) = parse_Add toks2 in
                (toks3, Sub (expr, expr_after_Add))
            
  |_ -> (toks_after_Mul, expr) 
  
and parse_Mul (toks : token list)  =
  let (toks_after_Pow, expr) = parse_Pow toks in 
  match ( lookahead toks_after_Pow ) with
  | Tok_Mult -> let toks2 = match_token toks_after_Pow Tok_Mult in
                let (toks3, expr_after_Mul) = parse_Mul toks2 in
                (toks3, Mult (expr, expr_after_Mul))
  |Tok_Div -> let toks2 = match_token toks_after_Pow Tok_Div in
                let (toks3, expr_after_Mul) = parse_Mul toks2 in
                (toks3, Div (expr, expr_after_Mul))
            
  |_ -> (toks_after_Pow, expr) 

and parse_Pow (toks : token list)  =
  let (toks_after_Una, expr) = parse_Una toks in 
  match ( lookahead toks_after_Una ) with
  | Tok_Pow -> let toks2 = match_token toks_after_Una Tok_Pow in
                let (toks3, expr_after_Pow) = parse_Pow toks2 in
                (toks3, Pow (expr, expr_after_Pow))
            
  |_ -> (toks_after_Una, expr)

and parse_Una (toks : token list)  =
  
  match ( lookahead toks ) with
  | Tok_Not -> let toks2 = match_token toks Tok_Not in
                let (toks3, expr_after_Una) = parse_Una toks2 in
                (toks3, Not (expr_after_Una))
            
  |_ -> parse_Prim toks

and parse_Prim (toks : token list)  =
  match lookahead toks with 
  | Tok_Int (i) -> let toks2 = match_token toks (Tok_Int i) in 
                   ( toks2, Int i)
  | Tok_Bool (b) -> let toks2 = match_token toks (Tok_Bool b) in 
                   ( toks2, Bool b)
  | Tok_ID (s) -> let toks2 = match_token toks (Tok_ID s) in 
                   ( toks2, ID s)                                  
  | Tok_LParen -> let toks2 = match_token toks (Tok_LParen) in 
                  let (toks3, expr) = parse_expr toks2 in
                  let toks4 = match_token toks3 Tok_RParen in
                  (toks4, expr)
  |_-> raise (InvalidInputException "oh")   

(****************************************************)
let rec parse_stmt toks : stmt_result =
  if lookahead toks = Tok_RBrace then (toks, NoOp)
  else
  let (remaining_tok, stmt) = parse_Opt toks in 

  match ( lookahead remaining_tok) with 
  | EOF -> (remaining_tok, Seq(stmt, NoOp))
  | Tok_RBrace -> (remaining_tok, Seq(stmt, NoOp))
  |_ -> let (remaining_tok2, stmt2) = parse_stmt remaining_tok in 
                         (remaining_tok2, Seq (stmt, stmt2))

and parse_Opt (toks : token list)  =
  match ( lookahead toks) with
  |Tok_Int_Type -> let (toks_after_Decl, str ) = parse_Decl (match_token toks Tok_Int_Type) in 
                    (toks_after_Decl , Declare ( Int_Type, str))
  |Tok_Bool_Type -> let (toks_after_Decl, str ) = parse_Decl (match_token toks Tok_Bool_Type) in 
                      (toks_after_Decl , Declare ( Bool_Type, str))
  |Tok_ID (s) -> let toks2 = match_token toks (Tok_ID (s)) in
                 let toks3 = match_token toks2 Tok_Assign in
                 let (toks_after_parseExp, expr) = parse_expr toks3 in 
                 let toks4 = match_token toks_after_parseExp (Tok_Semi) in
                 (toks4, Assign(s, expr))

  |Tok_Print -> let toks2 = match_token toks Tok_Print in
                let (toks_after_parseExp, expr) =  parse_expr toks2 in
                let toks3 = match_token toks_after_parseExp (Tok_Semi) in 
                (toks3, Print(expr)) 

  |Tok_If ->  let toks2 = match_token toks Tok_If in 
              let (toks_after_parseExp, expr) =  parse_expr toks2 in 
              let toks3 = match_token toks_after_parseExp (Tok_LBrace) in
              let (toks_after_parsestmt, stmt) =  parse_stmt toks3 in
              let toks4 = match_token toks_after_parsestmt (Tok_RBrace) in
              let (toks_after_parselse, stmt2) =  parse_Else toks4 in
              (toks_after_parselse, If (expr, stmt, stmt2))
  
  |Tok_For -> let toks2 = match_token toks Tok_For in 
               let toks3 = match_token toks2  Tok_LParen in
               let str = match ( lookahead toks3) with | Tok_ID (s) -> s | _-> raise (InvalidInputException "str in for") in
               let toks4 = match_token toks3 (Tok_ID (str)) in
               let toks5 = match_token toks4 Tok_From in
               let (toks_after_parseExp, expr1) =  parse_expr toks5 in 
               let toks6 = match_token toks_after_parseExp Tok_To in
               let (toks_after_parseExp2, expr2) =  parse_expr toks6 in
               let toks7 = match_token toks_after_parseExp2 Tok_RParen in
               let toks8 = match_token toks7 (Tok_LBrace) in
               let (toks_after_parsestmt, stmt) =  parse_stmt toks8 in
               let toks9 = match_token toks_after_parsestmt (Tok_RBrace) in
               (toks9, For(str, expr1, expr2, stmt))

  |Tok_While -> let toks2 = match_token toks Tok_While in
                let (toks_after_parseExp, expr) =  parse_expr toks2 in
                let toks3 = match_token toks_after_parseExp (Tok_LBrace) in
                let (toks_after_parsestmt, stmt) =  parse_stmt toks3 in
                let toks4 = match_token toks_after_parsestmt (Tok_RBrace) in
                (toks4, While(expr, stmt))


  |_ -> raise (InvalidInputException "not match stmt")

and parse_Decl toks = 
match ( lookahead toks) with
|Tok_ID (s) -> let toks2 = match_token toks (Tok_ID (s)) in
               let toks3 = match_token toks2 (Tok_Semi) in
                (toks3, s)
|_-> raise (InvalidInputException "not an ID")

and parse_Else toks = 
match ( lookahead toks) with
|Tok_Else -> let toks2 = match_token toks Tok_Else in
             let toks3 = match_token toks2 (Tok_LBrace) in
             let (toks_after_parsestmt, stmt) =  parse_stmt toks3 in
             let toks4 = match_token toks_after_parsestmt (Tok_RBrace) in
             (toks4, stmt)
|_-> (toks, NoOp)      


let parse_main toks : stmt =
  match ( lookahead toks) with
  |Tok_Int_Type  -> let toks2 = match_token toks Tok_Int_Type in
                    let toks3 = match_token toks2 Tok_Main in
                    let toks4 = match_token toks3 (Tok_LParen) in
                    let toks5 = match_token toks4 (Tok_RParen) in
                    let toks6 = match_token toks5 (Tok_LBrace) in
                    let (toks_after_parsestmt, stmt) =  parse_stmt toks6 in
                    let toks7 = match_token toks_after_parsestmt (Tok_RBrace) in
                    if  (match_token toks7 EOF) = [] then stmt
                    else raise (InvalidInputException "oh no")

  |_-> raise (InvalidInputException "in main")
