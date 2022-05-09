open Clear.Interpreter

let ty_to_string = function
  | LiteralT (Boolean x) -> Bool.to_string x
  | LiteralT (Integer x) -> Int.to_string x
  | _ -> "[NOT IMPLEMENTED]"

let print_ty chan v = output_string chan (ty_to_string v)

let () =
  eval (
      ApplicationT(
          AbstractionT('x', ArithmeticT(Addition, VariableT 'x', LiteralT(Integer 2))),
          LiteralT(Integer 1)
        )
    )
  |> (function
      | Ok x -> Printf.printf "%a\n" print_ty x
      | Error (msg: string) -> failwith msg)

  (* eval ( *)
  (*   ApplicationT( *)
  (*       AbstractionT('x', ArithmeticT(Division, VariableT 'x', LiteralT(Integer 0))), *)
  (*       LiteralT(Integer 1) *)
  (*     ) *)
  (* ) *)
  (* |> printf "%a" *)

  (* eval ( *)
  (*   ApplicationT( *)
  (*       AbstractionT( *)
  (*           'x', *)
  (*           ApplicationT( *)
  (*               AbstractionT( *)
  (*                       'y', *)
  (*                       ConditionT( *)
  (*                           VariableT 'x', *)
  (*                           ArithmeticT(Addition, VariableT 'y', LiteralT(Integer 1)), *)
  (*                           ArithmeticT(Subtraction, VariableT 'y', LiteralT(Integer 1)) *)
  (*                       ) *)
  (*                 ), *)
  (*               LiteralT(Integer 1) *)
  (*             ) *)
  (*         ), *)
  (*       LiteralT(Boolean true) *)
  (*     ) *)
  (* ) *)

  (*   |> printf "%a" *)

  (*   eval ( *)
  (*       ApplicationT( *)
  (*           AbstractionT( *)
  (*               'x', *)
  (*               ApplicationT( *)
  (*                   AbstractionT( *)
  (*                       'y', *)
  (*                       ConditionT( *)
  (*                           VariableT 'x', *)
  (*                           ArithmeticT(Addition, VariableT 'y', LiteralT(Integer 1)), *)
  (*                           ArithmeticT(Subtraction, VariableT 'y', LiteralT(Integer 1)) *)
  (*                       ) *)
  (*                   ), *)
  (*                   LiteralT(Integer 1) *)
  (*               ) *)
  (*           ), *)
  (*           LiteralT(Boolean false) *)
  (*       ) *)
  (*   ) *)

  (*   |> printf "%a" *)

  (*   eval ( *)
  (*       ApplicationT( *)
  (*           AbstractionT( *)
  (*               'x', *)
  (*               BooleanT( *)
  (*                   BooleanOperation.Or [ LiteralT(Boolean true) *)
  (*                                         LiteralT(Boolean false) *)
  (*                                         LiteralT(Boolean true) ] *)
  (*               ) *)
  (*           ), *)
  (*           LiteralT(Integer 1) *)
  (*       ) *)
  (*   ) *)
  (*   |> printf "%a" *)

  (*   eval ( *)
  (*       ApplicationT( *)
  (*           AbstractionT( *)
  (*               'x', *)
  (*               BooleanT( *)
  (*                   BooleanOperation.And [ LiteralT(Boolean true) *)
  (*                                          LiteralT(Boolean false) *)
  (*                                          LiteralT(Boolean true) ] *)
  (*               ) *)
  (*           ), *)
  (*           LiteralT(Integer 1) *)
  (*       ) *)
  (*   ) *)
  (*   |> printf "%a" *)


  (*   eval (BooleanT(BooleanOperation.Not(LiteralT(Boolean false)))) *)
  (*   |> printf "%a" *)
