open TokenTypes

let tokenize input =
  let regex_int = Str.regexp "-?[0-9]+" in
  let regex_bool = Str.regexp "true\\|false" in
  let regex_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let regex_nums = Str.regexp "[a-zA-Z0-9]+" in

  let rec tok pos s = 
    if pos >= String.length s then [EOF]
    else
      if (Str.string_match (Str.regexp "for") s pos) then
        let for_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
            let id_tok = Str.matched_string s in
          (Tok_ID (for_tok^id_tok))::(tok (Str.match_end()) s)
          else (Tok_For) :: (tok (pos+3) s)
      else if (Str.string_match regex_int s pos) then
        let int_tok = Str.matched_string s in
          (Tok_Int (int_of_string int_tok)) :: (tok (pos+(String.length int_tok)) s)
      else if (Str.string_match (Str.regexp "from") s pos) then
        let from_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
            let id_tok = Str.matched_string s in
          (Tok_ID (from_tok^id_tok))::(tok (Str.match_end()) s)
          else (Tok_From) :: (tok (pos+4) s)
      else if (Str.string_match (Str.regexp "to") s pos) then
        let to_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
            let id_tok = Str.matched_string s in
            (Tok_ID (to_tok^id_tok))::(tok (Str.match_end()) s)
          else (Tok_To) :: (tok (pos+2) s)
      else if (Str.string_match (Str.regexp "while") s pos) then
        let while_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
            let id_tok = Str.matched_string s in
          (Tok_ID (while_tok^id_tok))::(tok (Str.match_end()) s)
          else (Tok_While) :: (tok (pos+5) s)
      else if (Str.string_match (Str.regexp "int") s pos) then
        let int_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
            let id_tok = Str.matched_string s in
          (Tok_ID (int_tok^id_tok))::(tok (Str.match_end()) s)
          else (Tok_Int_Type) :: (tok (pos+3) s)
      else if (Str.string_match (Str.regexp "bool") s pos) then
        let bool_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
            let id_tok = Str.matched_string s in
          (Tok_ID (bool_tok^id_tok))::(tok (Str.match_end()) s)
          else (Tok_Bool_Type) :: (tok (pos+4) s)
      else if (Str.string_match (Str.regexp "-") s pos) then
        (Tok_Sub) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp ";") s pos) then
        (Tok_Semi) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp ")") s pos) then
        (Tok_RParen) :: (tok (pos+1) s)  
      else if (Str.string_match (Str.regexp "}") s pos) then
        (Tok_RBrace) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp "printf") s pos) then
        let print_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
           let id_tok = Str.matched_string s in
        (Tok_ID (print_tok^id_tok)):: (tok (Str.match_end()) s)
        else (Tok_Print) :: (tok (pos+6) s)
      else if (Str.string_match (Str.regexp "\\^") s pos) then
        (Tok_Pow) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp "+") s pos) then
        (Tok_Add) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp "||") s pos) then
        (Tok_Or) :: (tok (pos+2) s)
      else if (Str.string_match (Str.regexp "!=") s pos) then
        (Tok_NotEqual) :: (tok (pos+2) s)
      else if (Str.string_match (Str.regexp "!") s pos) then
        (Tok_Not) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp "*") s pos) then
        (Tok_Mult) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp "main") s pos) then
        let main_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
           let id_tok = Str.matched_string s in
        (Tok_ID (main_tok^id_tok)):: (tok (Str.match_end()) s)
        else (Tok_Main) :: (tok (pos+4) s)
      else if (Str.string_match (Str.regexp "<=") s pos) then
        (Tok_LessEqual) :: (tok (pos+2) s)
      else if (Str.string_match (Str.regexp "<") s pos) then
        (Tok_Less) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp "(") s pos) then
        (Tok_LParen) :: (tok (pos+1) s)
      else if (Str.string_match (Str.regexp "{") s pos) then
        (Tok_LBrace) :: (tok (pos+1) s)
      else if (Str.string_match(Str.regexp "if") s pos) then
        let if_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
           let id_tok = Str.matched_string s in
        (Tok_ID (if_tok^id_tok)):: (tok (Str.match_end()) s)
        else (Tok_If) :: (tok (pos+2) s)
      else if (Str.string_match (Str.regexp ">=") s pos) then
        (Tok_GreaterEqual) :: (tok (pos+2) s)
      else if(Str.string_match (Str.regexp ">") s pos) then 
        (Tok_Greater) :: (tok (pos+1) s)
      else if(Str.string_match (Str.regexp "==") s pos) then 
        (Tok_Equal) :: (tok (pos+2) s)
      else if(Str.string_match (Str.regexp "else") s pos) then
        let else_tok = Str.matched_string s in
        let new_pos = Str.match_end() in
          if (Str.string_match regex_nums s new_pos) then
           let id_tok = Str.matched_string s in
        (Tok_ID (else_tok^id_tok)):: (tok (Str.match_end()) s) 
        else (Tok_Else) :: (tok (pos+4) s)
      else if(Str.string_match (Str.regexp "/") s pos) then 
        (Tok_Div) :: (tok (pos+1) s)
      else if (Str.string_match regex_bool s pos) then 
        let bool_tok = Str.matched_string s in
          (Tok_Bool (bool_tok = "true"))::(tok (pos+(String.length bool_tok)) s)
      else if(Str.string_match (Str.regexp "=") s pos) then 
        (Tok_Assign) :: (tok (pos+1) s)
      else if(Str.string_match (Str.regexp "&&") s pos) then 
        (Tok_And) :: (tok (pos+2) s)
      else if(Str.string_match (Str.regexp "[ \t\n]") s pos) then 
        let space_tok = Str.matched_string s in
          (tok (pos+(String.length space_tok)) s)
      else if (Str.string_match regex_id s pos) then
        let id_tok = Str.matched_string s in
          (Tok_ID id_tok) :: (tok (pos+(String.length id_tok)) s)
      else
        raise (InvalidInputException "TokenizeFailure")
    in
      tok 0 input




