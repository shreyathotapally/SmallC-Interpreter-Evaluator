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

    let rec parse_expr toks = 
      parse_or toks

    and parse_or lst = 
      let (lst2, e1) = (parse_and lst) in
        match lst2 with
        | Tok_Or :: tl ->
          let lst3 = (match_token lst2 Tok_Or) in 
          let (lst4, e2) = (parse_or lst3) in
           (lst4, Or(e1,e2))
        | _ -> (lst2, e1)

    and parse_and lst = 
      let (lst2, e1) = (parse_equality lst) in
        match lst2 with
        | Tok_And :: tl ->
          let lst3 = (match_token lst2 Tok_And) in 
          let (lst4, e2) = (parse_and lst3) in
           (lst4, And(e1,e2))
        | _ -> (lst2, e1)

    and parse_equality lst = 
      let (lst2, e1) = (parse_relational lst) in
        match lst2 with
        | Tok_Equal :: tl ->
          let lst3 = (match_token lst2 Tok_Equal) in
          let (lst4, e2) = (parse_equality lst3) in
           (lst4, Equal(e1,e2))
        | Tok_NotEqual :: tl ->
          let lst3 = (match_token lst2 Tok_NotEqual) in
          let (lst4, e2) = (parse_equality lst3) in
           (lst4, NotEqual(e1,e2))
        | _ -> (lst2, e1)

    and parse_relational lst = 
      let (lst2, e1) = (parse_additive lst) in
        match lst2 with
    
        | Tok_Less :: tl ->
          let lst3 = (match_token lst2 Tok_Less) in
          let (lst4, e2) = (parse_relational lst3) in
           (lst4, Less(e1,e2))
    
        | Tok_Greater :: tl ->
          let lst3 = (match_token lst2 Tok_Greater) in
          let (lst4, e2) = (parse_relational lst3) in 
            (lst4, Greater(e1,e2))
    
        | Tok_LessEqual :: tl -> 
          let lst3 = (match_token lst2 Tok_LessEqual) in
          let (lst4, e2) = (parse_relational lst3) in
           (lst4, LessEqual(e1,e2))
    
        | Tok_GreaterEqual :: tl -> 
          let lst3 = (match_token lst2 Tok_GreaterEqual) in
          let (lst4, e2) = (parse_relational lst3) in
           (lst4, GreaterEqual(e1,e2))
    
        | _ -> (lst2, e1) 

    and parse_additive lst = 
      let (lst2, e1) = (parse_mult lst) in
        match lst2 with
        | Tok_Add :: tl -> 
          let lst3 = (match_token lst2 Tok_Add) in
          let (lst4, e2) = (parse_additive lst3) in
           (lst4, Add(e1,e2))
    
        | Tok_Sub :: tl -> 
          let lst3 = (match_token lst2 Tok_Sub) in
          let (lst4, e2) = (parse_additive lst3) in
           (lst4, Sub(e1,e2))
    
        | _ -> (lst2, e1) 

    and parse_mult lst = 
      let (lst2, e1) = (parse_power lst) in
        match lst2 with
        | Tok_Mult :: tl -> 
          let lst3 = (match_token lst2 Tok_Mult) in 
          let (lst4, e2) = (parse_mult lst3) in
          (lst4, Mult(e1,e2))
    
        | Tok_Div :: tl -> 
          let lst3 = (match_token lst2 Tok_Div) in 
          let (lst4, e2) = (parse_mult lst3) in
           (lst4, Div(e1,e2))
        | _ -> (lst2, e1) 

    and parse_power lst = 
      let (lst2, e1) = (parse_unary lst) in
        match lst2 with
        | Tok_Pow :: tl -> 
          let lst3 = (match_token lst2 Tok_Pow) in 
          let (lst4, e2) = (parse_power lst3) in
           (lst4, Pow(e1,e2))
        | _ -> (lst2, e1)

    and parse_unary lst = 
      match lst with
      | Tok_Not :: tl -> 
        let lst2 = (match_token lst Tok_Not) in
        let (lst3, e2) = (parse_unary lst2) in
         (lst3, Not(e2))
      | _ -> (parse_primary lst)
    
    and parse_primary lst = 
      match lst with
    
      | (Tok_Int(x)) :: tl -> 
        let lst2 = (match_token lst (Tok_Int(x))) in
        (lst2, Int(x))
    
      | (Tok_Bool(x)) :: tl ->
        let lst2 = (match_token lst (Tok_Bool(x))) in
        (lst2, Bool x)
    
      | (Tok_ID(x)) :: tl -> 
        let lst2 = (match_token lst (Tok_ID(x))) in
        (lst2, ID x)
    
      | Tok_LParen :: tl -> 
        let lst2 = (match_token lst Tok_LParen) in
        (let (lst3, e2) = (parse_expr lst2) in
        match lst3 with
          | Tok_RParen::t -> (t, e2)
          | _ -> raise (InvalidInputException("No right parenthesis")))
      | _ -> let x = (List.hd lst) in
        raise (InvalidInputException("x"))
    ;;

    let rec parse_stmt toks = 
      match toks with
    
      | Tok_Int_Type :: tl -> 
        let (lst, dec) = (declareStatement toks) in
        let (lst2, stmt) = (parse_stmt lst) in
        (lst2, Seq(dec, stmt))
    
      | Tok_Bool_Type :: tl -> 
        let (lst, bool) = (declareStatement toks) in
        let (lst2, stmt) = (parse_stmt lst) in
        (lst2, Seq(bool, stmt))
    
      | Tok_ID(x) :: tl -> 
        let (lst, id) = (assignStatement toks) in
        let (lst2, stmt) = (parse_stmt lst) in
        (lst2, Seq(id, stmt))
    
      | Tok_Print :: tl -> 
        let (lst, print) = (printStatement toks) in
        let (lst2, stmt) = (parse_stmt lst) in
        (lst2, Seq(print, stmt))
    
      | Tok_If :: tl -> 
        let (lst, if_stmt) = (ifStatement toks) in
        let (lst2, stmt) = (parse_stmt lst) in
        (lst2, Seq(if_stmt, stmt))
    
      | Tok_While :: tl -> 
        let (lst, while_stmt) = (whileStatement toks) in
        let (lst2, stmt) = (parse_stmt lst) in
        (lst2, Seq(while_stmt, stmt))
      
      | Tok_For :: tl -> 
        let (lst, for_stmt) = (forStatement toks) in
        let (lst2, stmt) = (parse_stmt lst) in
        (lst2, Seq(for_stmt, stmt))  
    
      | _ -> (toks, NoOp)

    and declareStatement lst = match lst with
      | Tok_Int_Type :: tl -> 
        let lst2 = (match_token lst Tok_Int_Type) in
        (match lst2 with
          | (Tok_ID(x)) :: tl ->   
            let lst3 = (match_token lst2 (Tok_ID(x))) in
            let lst4 = (match_token lst3 Tok_Semi) in
            (lst4, Declare(Int_Type, x))
          | _ -> raise (InvalidInputException("Int Declare Error")))
      
      | Tok_Bool_Type :: tl -> 
        let lst2 = (match_token lst Tok_Bool_Type) in
        (match lst2 with
          | (Tok_ID(x)) :: tl ->
            let lst3 = (match_token lst2 (Tok_ID(x))) in
            let lst4 = (match_token lst3 Tok_Semi) in
            (lst4, Declare(Bool_Type, x))
          | _ -> raise (InvalidInputException("Bool Declare Error")))
      | _ -> raise (InvalidInputException("Declare Error"))

    and assignStatement lst = match lst with
      | (Tok_ID(x)) :: tl -> 
        let lst2 = (match_token lst (Tok_ID(x))) in
        let lst3 = (match_token lst2 Tok_Assign) in
        let (lst4, exp) = (parse_expr lst3) in 
        let lst5 = (match_token lst4 Tok_Semi) in
        (lst5, Assign(x, exp))
      | _ -> raise (InvalidInputException("Assign Error"))

    and printStatement lst = match lst with
      | Tok_Print :: tl -> 
          let lst2 = (match_token lst Tok_Print) in
          let lst3 = (match_token lst2 Tok_LParen) in
          let (lst4, exp) = (parse_expr lst3) in
          let lst5 = (match_token lst4 Tok_RParen) in 
          let lst6 = (match_token lst5 Tok_Semi) in
          (lst6, Print(exp))
      | _ -> raise (InvalidInputException("Print Error"))

    and ifStatement lst = match lst with
      | Tok_If :: tl -> 
        let lst2 = (match_token lst Tok_If) in
        let lst3 = (match_token lst2 Tok_LParen) in
        let (lst4, if_exp) = (parse_expr lst3) in
        let lst5 = (match_token lst4 Tok_RParen) in

        let lst6 = (match_token lst5 Tok_LBrace) in
        let (lst7, if_stmt) = (parse_stmt lst6) in
        let lst8 = (match_token lst7 Tok_RBrace) in

        (match lst8 with
        | Tok_Else :: tl -> 
          let lst9 = (match_token lst8 Tok_Else) in
          let lst10 = (match_token lst9 Tok_LBrace) in
          let (lst11, else_stmt) = (parse_stmt lst10) in
          let lst12 = (match_token lst11 Tok_RBrace) in
          (lst12, If(if_exp, if_stmt, else_stmt))  
        | _ -> (lst8, If(if_exp, if_stmt, NoOp)))
      
      | _ -> raise (InvalidInputException("If Error"))
      
    and forStatement lst = match lst with
    | Tok_For :: tl -> 
        let lst2 = (match_token lst Tok_For) in
        let lst3 = (match_token lst2 Tok_LParen) in
        (match lst3 with
          | (Tok_ID(x)) :: tl ->   
            let lst4 = (match_token lst3 (Tok_ID(x))) in
            let lst5 = (match_token lst4 Tok_From) in
            let (lst6,start) = (parse_expr lst5) in 
            let lst7 = (match_token lst6 Tok_To) in
            let (lst8,last) = (parse_expr lst7) in
            let lst9 = (match_token lst8 Tok_RParen) in
            let lst10 = (match_token lst9 Tok_LBrace) in
            let (lst11, stmt) = (parse_stmt lst10) in
            let lst12 = (match_token lst11 Tok_RBrace) in
             (lst12, For(x,start,last,stmt))	
            | _ -> raise (InvalidInputException("For Error")))
    
    and whileStatement lst = match lst with		
      | Tok_While :: tl -> 
        let lst2 = (match_token lst Tok_While) in
        let lst3 = (match_token lst2 Tok_LParen) in
        let (lst4, exp) = (parse_expr lst3) in
        let lst5 = (match_token lst4 Tok_RParen) in
        let lst6 = (match_token lst5 Tok_LBrace) in
        let (lst7, stmt) = (parse_stmt lst6) in
        let lst8 = (match_token lst7 Tok_RBrace) in
         (lst8, While(exp, stmt))	
      | _ -> raise (InvalidInputException("While Error"))
    ;;
    
    let parse_main toks = 
      let lst = (match_token toks Tok_Int_Type) in
      let lst1 = (match_token lst Tok_Main) in
      let lst2 = (match_token lst1 Tok_LParen) in
      let lst3 = (match_token lst2 Tok_RParen) in 
      let lst4 = (match_token lst3 Tok_LBrace) in 
      let (lst4, stmt) = (parse_stmt lst4) in
      
      let lst5 = (match_token lst4 Tok_RBrace) in
      let lst6 = (match_token lst5 EOF) in stmt 
    ;;