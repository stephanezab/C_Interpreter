open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let power x y =
  let ele = (float_of_int x) ** (float_of_int y) in 
  let ele2 = floor ele in 
  int_of_float ele2

let rec eval_expr env t =
  match t with 
  | Int i -> Int_Val i
  | Bool b -> Bool_Val b
  | ID s -> if (List.mem_assoc s env) = true then List.assoc s env else raise (DeclareError("ID")) (* <-- do that mem_ass thing here *)
  | Add (e1, e2) -> let t1 = eval_expr env e1 in 
                    let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in add")) in
                    let t2 = eval_expr env e2 in
                    let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in add")) in
                    let n3 = n1+n2 in
                    Int_Val n3 

  |Mult  (e1, e2) -> let t1 = eval_expr env e1 in 
                     let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in mult")) in
                     let t2 = eval_expr env e2 in
                     let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in mult")) in
                     let n3 = n1*n2 in
                     Int_Val n3 

  |Sub  (e1, e2) -> let t1 = eval_expr env e1 in 
                    let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in sub")) in
                    let t2 = eval_expr env e2 in
                    let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in sub")) in
                    let n3 = n1-n2 in
                    Int_Val n3 

  |Div  (e1, e2) -> let t1 = eval_expr env e1 in 
                    let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in div")) in
                    let t2 = eval_expr env e2 in
                    let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in div")) in
                    if n2 = 0 then raise (DivByZeroError) else let n3 = n1/n2 in
                    Int_Val n3 

  |Pow  (e1, e2) -> let t1 = eval_expr env e1 in 
                    let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in pow")) in
                    let t2 = eval_expr env e2 in
                    let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in pow")) in
                    let n3 = power n1 n2 in
                    Int_Val n3

  |And  (e1, e2) -> let t1 = eval_expr env e1 in 
                    let b1 = match t1 with | Bool_Val b -> b | _-> raise (TypeError("in and")) in
                    let t2 = eval_expr env e2 in
                    let b2 = match t2 with | Bool_Val b -> b | _-> raise (TypeError("in and")) in
                    let b3 = if b1 && b2 then Bool_Val true else Bool_Val false in 
                    b3
  
  | Or  (e1, e2) -> let t1 = eval_expr env e1 in 
                     let b1 = match t1 with | Bool_Val b -> b | _-> raise (TypeError("in Or")) in
                     let t2 = eval_expr env e2 in
                     let b2 = match t2 with | Bool_Val b -> b | _-> raise (TypeError("in Or")) in
                     let b3 = if b1 || b2 then Bool_Val true else Bool_Val false in
                     b3
  
  | Not e1 -> let t1 = eval_expr env e1 in 
              let b1 = match t1 with | Bool_Val b -> b | _-> raise (TypeError("in Not")) in
              let b3 = if b1 then Bool_Val false else Bool_Val true in 
              b3

  | Greater (e1, e2) -> let t1 = eval_expr env e1 in 
                        let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in Greater")) in
                        let t2 = eval_expr env e2 in
                        let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in Greater")) in
                        if n1 > n2 then Bool_Val true else Bool_Val false

  |GreaterEqual (e1, e2) -> let t1 = eval_expr env e1 in 
                            let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in GreaterEqual")) in
                            let t2 = eval_expr env e2 in
                            let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in GreaterEqual")) in
                            if n1 >= n2 then Bool_Val true else Bool_Val false

  |Less (e1, e2) -> let t1 = eval_expr env e1 in 
                    let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in less")) in
                    let t2 = eval_expr env e2 in
                    let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in less")) in
                    if n1 < n2 then Bool_Val true else Bool_Val false

  | LessEqual (e1, e2) -> let t1 = eval_expr env e1 in 
                          let n1 = match t1 with | Int_Val i -> i | _-> raise (TypeError("in lessEqual")) in
                          let t2 = eval_expr env e2 in
                          let n2 = match t2 with | Int_Val i -> i | _-> raise (TypeError("in lessEqual")) in
                          if n1 <= n2 then Bool_Val true else Bool_Val false 

  |Equal (e1, e2) -> let t1 = eval_expr env e1 in   
                     let t2 = eval_expr env e2 in
                     let a = match (t1,t2) with 
                     | (Bool_Val b1 , Bool_Val b2 ) -> if b1 == b2 then Bool_Val true else Bool_Val false
                     | (Int_Val n1 , Int_Val n2 ) -> if n1 == n2 then Bool_Val true else Bool_Val false
                     | _->  raise (TypeError("in Equal")) in 
                     a 

  | NotEqual (e1, e2) -> let t1 = eval_expr env e1 in   
                         let t2 = eval_expr env e2 in
                         let a = match (t1,t2) with 
                         | (Bool_Val b1 , Bool_Val b2 ) -> if b1 != b2 then Bool_Val true else Bool_Val false
                         | (Int_Val n1 , Int_Val n2 ) -> if n1 != n2 then Bool_Val true else Bool_Val false
                         | _->  raise (TypeError("in NotEqual"))  in 
                         a                                                                        

let rec eval_stmt env s =
  match s with 
  | NoOp -> env
  | Seq ( stmt1 , stmt2) -> let env2 = eval_stmt env stmt1 in 
                          let env3 = eval_stmt env2 stmt2 in 
                          env3
  | Declare (t, s) -> let value = match t with |Int_Type -> Int_Val(0) |Bool_Type -> Bool_Val(false) in
                         if (List.mem_assoc s env)= false then  env@[(s, value)]   else  raise (DeclareError" in declare")
  
  | Assign (s, e) ->  if List.mem_assoc s env then 
                              let v = eval_expr env e in 
                              let typeofs = List.assoc s env in 
                              let env3 = match (typeofs, v) with 
                                        | (Int_Val n1 , Int_Val n2 ) -> let env2 = List.remove_assoc s env in env2@[(s, Int_Val n2 )]
                                        | (Bool_Val b1 , Bool_Val b2 ) -> let env2 = List.remove_assoc s env in env2@[(s, Bool_Val b2 )]
                                        | _-> raise (TypeError("in typedeclare"))    in        
                                                     
                                       env3            

                      else raise (DeclareError" in declare") 

  | If (e, stmt1, stmt2) ->  let bool_value = eval_expr env e in 
                           let value = match bool_value with | Bool_Val b -> b | _-> raise (TypeError("in If")) in 
                           if value = true then (eval_stmt env stmt1) else (eval_stmt env stmt2)

  | While (e, stmt) ->  while_loop e env stmt
  
  | For (s, e1, e2, stmt) ->  let v1 = eval_expr env e1 in 
                              let n1 = match v1 with | Int_Val i -> i | _-> raise (TypeError("in for")) in
                              let v2 = eval_expr env e2 in
                              let n2 = match v2 with | Int_Val i -> i | _-> raise (TypeError("in for")) in
                              let env2 = List.remove_assoc s env in 
                              let env3 = env2@[(s,Int_Val n1)] in 
                              let env4 = eval_stmt env3 stmt in 
                                for_loop s env4 n2 stmt

  | Print (e) -> let v = eval_expr env e in
                 match v with 
                 | Int_Val i -> print_output_int i; print_output_newline(); env
                 | Bool_Val b -> print_output_bool b; print_output_newline(); env
                                


and while_loop e env stmt  =
 let bool_value = eval_expr env e in
 match bool_value with 
 | Bool_Val b -> if b = true then let new_env = eval_stmt env stmt in 
                                  while_loop e new_env stmt 
                 else env                 
                                 
 | _-> raise (TypeError("in while"))

and for_loop s env limit stmt = 
 let int_value = List.assoc s env in 
 let count = match int_value with | Int_Val i -> i+1 | _-> raise (TypeError("in for")) in
 if count > limit then let env2 = List.remove_assoc s env in
                       env2@[(s, Int_Val count)]

 else let env2 = List.remove_assoc s env in
      let env3 = env2@[(s, Int_Val count)] in 
      let new_env = eval_stmt env3 stmt in 
        for_loop s new_env limit stmt  
