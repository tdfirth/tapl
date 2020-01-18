open Core

exception InvalidEvaluator

let command =
  Command.basic ~summary:"Run and print Arith files."
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type)
      and evaluator =
        flag "-e"
          (optional_with_default "small" string)
          ~doc:"string  The evaluator to use."
      in
      fun () ->
        let m =
          match evaluator with
          | "small" ->
              Printf.printf "Using the SmallStep Evaluator:\n";
              ( module Meta.MakeInterpreter (Arith.MakeArith (Arith.SmallStep))
              : Meta.Interpreter )
          | "big" ->
              Printf.printf "Using the BigStep Evaluator:\n";
              ( module Meta.MakeInterpreter (Arith.MakeArith (Arith.BigStep))
              : Meta.Interpreter )
          | _ -> raise InvalidEvaluator
        in
        let module I = (val m) in
        let prog = I.interpret filename in
        I.print_program prog)

let () = Command.run ~version:"1.0" command
