open Core

exception InvalidEvaluator

let interpret_file (module I : Meta.Interpreter) file =
  let inx = In_channel.create file in
  In_channel.input_all inx |> I.read ~origin:file |> I.eval |> I.print

exception EmptyInput

let repl (module I : Meta.Interpreter) =
  let rec loop () =
    Out_channel.output_string Out_channel.stdout ">>> ";
    Out_channel.flush Out_channel.stdout;
    try
      let input =
        match In_channel.input_line In_channel.stdin with
        | Some i -> i
        | None -> raise EmptyInput
      in
      input |> I.read ~origin:"repl" |> I.eval |> I.print |> loop
    with EmptyInput -> loop ()
  in
  loop ()

let command =
  Command.basic ~summary:"Run and print Arith files."
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open filename = anon (maybe ("filename" %: Filename.arg_type))
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
          | "untyped" ->
              Printf.printf "Using the Untyped Language:\n";
              (module Meta.MakeInterpreter (Untyped.Untyped) : Meta.Interpreter)
          | _ -> raise InvalidEvaluator
        in
        match filename with
        | Some file -> interpret_file m file
        | None -> repl m)

let () = Command.run ~version:"1.0" command
