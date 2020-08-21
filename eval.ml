open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  match t with 
  | Int x -> Int_Val x
  | Bool x -> Bool_Val x
  | ID x when List.mem_assoc x env -> List.assoc x env
  | ID _ -> raise (DeclareError "Identifier has no binding")
  | Add(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with 
                       | Int_Val y1 -> Int_Val(x1 + y1)
                       | _ -> raise (TypeError "Non-integer"))
        | _ -> raise (TypeError "Non-integer")               
        )
  | Sub(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with 
                       | Int_Val y1 -> Int_Val(x1 - y1)
                       | _ -> raise (TypeError "Non-integer"))
        | _ -> raise (TypeError "Non-integer")               
        )
  | Mult(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with 
                       | Int_Val y1 -> Int_Val(x1 * y1)
                       | _ -> raise (TypeError "Non-integer"))
        | _ -> raise (TypeError "Non-integer")               
        )
  | Div(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with 
                       | Int_Val y1 when y1 = 0 -> raise (DivByZeroError)
                       | Int_Val y1 -> Int_Val(x1 / y1)
                       | _ -> raise (TypeError "Non-integer value"))
        | _ -> raise (TypeError "Non-integer value")               
        )
  | Pow(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with 
                       | Int_Val y1 -> Int_Val (int_of_float ((float_of_int x1)**(float_of_int y1)))
                       | _ -> raise (TypeError "Non-integer value"))
        | _ -> raise (TypeError "Non-integer value")               
        )
  | Or(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Bool_Val x1 -> (match eval_y with
                        | Bool_Val y1 when x1 = false && y1 = false -> Bool_Val false
                        | Bool_Val y1 -> Bool_Val true
                        | _ -> raise (TypeError "Non-bool value"))
        | _ -> raise (TypeError "Non-bool value")
        )
  | And(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Bool_Val x1 -> (match eval_y with
                        | Bool_Val y1 when x1 = true && y1 = true -> Bool_Val true
                        | Bool_Val y1 -> Bool_Val false
                        | _ -> raise (TypeError "Non-bool value"))
        | _ -> raise (TypeError "Non-bool value")
        )
  | Not x -> let eval_x = eval_expr env x in
        (match eval_x with 
        | Bool_Val x1 when x1 = false -> Bool_Val true
        | Bool_Val x1 when x1 = true -> Bool_Val false
        | _ -> raise (TypeError "Non-bool value")
        )
  | Greater(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with
                        | Int_Val y1 when x1 > y1 -> Bool_Val true
                        | Int_Val y1 when x1 <= y1 -> Bool_Val false
                        | _ -> raise (TypeError "Non-int value"))
        | _ -> raise (TypeError "Non-int value")
        )
  | Less(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with
                        | Int_Val y1 when x1 < y1 -> Bool_Val true
                        | Int_Val y1 when x1 >= y1 -> Bool_Val false
                        | _ -> raise (TypeError "Non-int value"))
        | _ -> raise (TypeError "Non-int value")
        )
  | GreaterEqual(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with
                        | Int_Val y1 when x1 >= y1 -> Bool_Val true
                        | Int_Val y1 when x1 < y1 -> Bool_Val false
                        | _ -> raise (TypeError "Non-int value"))
        | _ -> raise (TypeError "Non-int value")
        )
  | LessEqual(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with
                        | Int_Val y1 when x1 <= y1 -> Bool_Val true
                        | Int_Val y1 when x1 > y1 -> Bool_Val false
                        | _ -> raise (TypeError "Non-int value"))
        | _ -> raise (TypeError "Non-int value")
        )
  | Equal(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with
                        | Int_Val y1 when x1 == y1 -> Bool_Val true
                        | Int_Val y1 when x1 != y1 -> Bool_Val false
                        | _ -> raise (TypeError "Varying types"))
        |Bool_Val x1 -> (match eval_y with 
                        | Bool_Val y1 when x1 == y1 -> Bool_Val true
                        | Bool_Val y1 when x1 != y1 -> Bool_Val false
                        | _ -> raise (TypeError "Varying types"))           
        )  
  | NotEqual(x, y) -> let eval_x = eval_expr env x in 
        let eval_y = eval_expr env y in
        (match eval_x with
        |Int_Val x1 -> (match eval_y with
                        | Int_Val y1 when x1 != y1 -> Bool_Val true
                        | Int_Val y1 when x1 == y1 -> Bool_Val false
                        | _ -> raise (TypeError "Varying types"))
        |Bool_Val x1 -> (match eval_y with 
                        | Bool_Val y1 when x1 != y1 -> Bool_Val true
                        | Bool_Val y1 when x1 == y1 -> Bool_Val false
                        | _ -> raise (TypeError "Varying types"))                
        ) 
        
  
let rec eval_stmt env s =
  match s with 
  | NoOp -> env
  | Seq(stmt1, stmt2) -> eval_stmt(eval_stmt env stmt1) stmt2
  | Declare (x, stmt) -> (if (List.mem_assoc stmt env) then
     raise (DeclareError "Already been declared")
				else match x with
			    		| Int_Type -> (stmt, Int_Val(0)) :: env
              | Bool_Type -> (stmt, Bool_Val false) :: env)
  | Assign (stmt, expr) -> (if (List.mem_assoc stmt env) then
					match (List.assoc stmt env,eval_expr env expr) with
					| (Int_Val _, Int_Val x) -> ((stmt, Int_Val x) :: (List.remove_assoc stmt env))
					| (Bool_Val _, Bool_Val x) -> ((stmt, Bool_Val x) :: (List.remove_assoc stmt env))
					| _ -> raise (TypeError "This variable has a different type")
        else raise (DeclareError "This variable has not been declared"))
  | If (guard, if_stmt, else_stmt) -> let eval_guard = eval_expr env guard in 
        (match eval_guard with
			  | Bool_Val true -> (eval_stmt env if_stmt)
				| Bool_Val false -> (eval_stmt env else_stmt)
				| _ -> raise (TypeError "Guard does not evaluate to boolean"))
  | While (guard, body) -> let eval_guard = eval_expr env guard in 
        (match eval_guard with
        | Bool_Val true -> eval_stmt (eval_stmt env body) (While (guard, body))
				| Bool_Val false -> env
        | _ -> raise (TypeError "Guard does not evaluate to boolean"))
  |For(id, starting, ending, body) -> let eval_s = eval_expr env starting in 
      let eval_e = eval_expr env ending in 
      let id_env = ID id in 
      eval_expr (env Assign(id_env, starting))

  | Print expr -> let eval_exp = eval_expr env expr in 
      (match eval_exp with
			| Int_Val x -> (let _ = (print_output_int x) in let _ = (print_output_string "\n") in env)
			| Bool_Val x -> (let _ = (print_output_bool x) in let _ = (print_output_string "\n") in env))
