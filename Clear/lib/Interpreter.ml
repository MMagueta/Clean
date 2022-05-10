type expression =
    | LiteralT of primitive
    | VariableT of char
    | AbstractionT of char * expression
    | ClosureT of char * expression * env
    | ApplicationT of expression * expression
    | ArithmeticT of arithmeticOperation * expression * expression
    | BooleanT of booleanOperation
    | ConditionT of expression * expression * expression

and env = (char * expression) list

and arithmeticOperation =
    | Addition
    | Subtraction
    | Division
    | Multiplication

and booleanOperation =
    | GreaterThan of expression * expression
    | LesserThan of expression * expression
    | Equals of expression * expression
    | Different of expression * expression
    | Not of expression
    | Or of expression list
    | And of expression list

and primitive =
    | Boolean of bool
    | Integer of int
    | Float of float
    | String of string
    | Character of char
    | List of primitive list

and environment = (char*expression) list

let rec eval_with_environment (env: environment) (exp: expression): (expression, string) result =
  match exp with
  | LiteralT _ -> Ok exp
  | ArithmeticT (op, left, right) ->
     arithmetic_eval op env left right
  | BooleanT op ->
     booleanEval op env
  | ConditionT (condition, trueCase, falseCase) ->
     begin
       match eval_with_environment env condition with
       | Ok (LiteralT (Boolean true)) -> eval_with_environment env trueCase
       | Ok (LiteralT (Boolean false)) -> eval_with_environment env falseCase
       | Ok _ -> Error ""
       | Error msg -> Error msg
     end
  | VariableT name ->
     env
     |> List.find_opt (fun (label, _) -> label = name)
     |> Option.map snd
     |> fun x -> Option.to_result ~none:"" x
  | AbstractionT (arg, body) ->
     Ok (ClosureT (arg, body, env))
  | ApplicationT (f, value) -> applicationEval env f value
  | closure -> Ok closure

and booleanEval op env =
  match op with
  | Or elementsToOr ->
     let evaluatedList = List.map (eval_with_environment env) elementsToOr in       
     let anyFailure =
       (evaluatedList
        |> List.find_opt (function | Error _ -> true | _ -> false)
        |> function
          | None -> false
          | Some _ -> true) in
     if not anyFailure then
       evaluatedList
       |> List.find_opt (function | Ok (LiteralT(Boolean true)) -> true | _ -> false)
       |> function
         | Some _ -> Ok (LiteralT (Boolean true))
         | None -> Error ""
     else
       Error ""
  | And elementsToOr ->
     let evaluatedList = List.map (eval_with_environment env) elementsToOr in
     let anyFailure =
       (evaluatedList
        |> List.find_opt (function | Error _ -> true | _ -> false)
        |> function
          | None -> false
          | Some _ -> true) in
     if not anyFailure then
       evaluatedList
       |> List.for_all (function | Ok (LiteralT(Boolean true)) -> true | _ -> false)
       |> function
         | true -> Ok (LiteralT (Boolean true))
         | false -> Error ""
     else
       Error ""
  | Not elementToNegate ->
     begin
       match (eval_with_environment env elementToNegate) with
       | Ok (LiteralT (Boolean elem)) -> Ok(LiteralT(Boolean(not elem)))
       | Ok _ -> Error ""
       | Error msg -> Error msg
     end
  | GreaterThan (left, right) ->
     begin
       match ((eval_with_environment env left), (eval_with_environment env right)) with
       | Ok (LiteralT (Integer left)), Ok (LiteralT (Integer right)) ->
          if left > right then
            Ok(LiteralT(Boolean true))
          else
            Ok(LiteralT(Boolean false))
       | _, Error msg
       | Error msg, _ -> Error msg
       | _, _ -> Error ""
     end
  | LesserThan (left, right) ->
     begin
         match ((eval_with_environment env left), (eval_with_environment env right)) with
           | Ok (LiteralT (Integer left)), Ok (LiteralT (Integer right)) ->
              if left < right then
                Ok(LiteralT(Boolean true))
              else
                Ok(LiteralT(Boolean false))
           | _, Error msg
           | Error msg, _ -> Error msg
           | _, _ -> Error ""
     end
  | Equals (left, right) ->
     begin
         match ((eval_with_environment env left), (eval_with_environment env right)) with
           | Ok (LiteralT left), Ok (LiteralT right) ->
              if left = right then
                Ok(LiteralT(Boolean true))
              else
                Ok(LiteralT(Boolean false))
           | _, Error msg
           | Error msg, _ -> Error msg
           | _, _ -> Error ""
     end
  | Different (left, right) ->
     begin
         match ((eval_with_environment env left), (eval_with_environment env right)) with
           | Ok (LiteralT left), Ok (LiteralT right) ->
              if left <> right then
                Ok(LiteralT(Boolean true))
              else
                Ok(LiteralT(Boolean false))
           | _, Error msg
           | Error msg, _ -> Error msg
           | _, _ -> Error ""
     end

and arithmetic_eval op env left right =
  match (eval_with_environment env left, eval_with_environment env right) with
  | Ok (LiteralT (Integer evaluatedLeft)), Ok (LiteralT (Integer evaluatedRight)) ->
     begin
       match op with
       | Addition ->
          Ok (LiteralT (Integer (evaluatedLeft + evaluatedRight)))
       | Subtraction ->
          Ok (LiteralT (Integer (evaluatedLeft - evaluatedRight)))
       | Division when evaluatedRight <> 0 ->
          Ok (LiteralT (Integer (evaluatedLeft / evaluatedRight)))
       | Multiplication ->
          Ok (LiteralT (Integer (evaluatedLeft * evaluatedRight)))
       | _ -> Error ""
     end
  | _ -> Error ""

and closureEval arg body closedEnv env value =
  match eval_with_environment env value with
  | Ok evaluatedValue ->
     let newEnv = ((arg, evaluatedValue)::closedEnv) @ env in
     eval_with_environment newEnv body
  | _ -> Error "TODO ERROR MESSAGE"

and applicationEval (env) (f) (value) =
  match eval_with_environment env f with
  | Ok (ClosureT (arg, body, closedEnv)) -> closureEval arg body closedEnv env value
  | _ -> Error "TODO ERROR MESSAGE";;

let eval (exp: expression): (expression, string) result=
  eval_with_environment [] exp
