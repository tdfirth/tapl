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

let make_lang_table =
  let table = Hashtbl.create (module String) in
  Hashtbl.add_exn table ~key:"small"
    ~data:
      ( module Meta.MakeInterpreter (Arith.MakeArith (Arith.SmallStep))
      : Meta.Interpreter );
  Hashtbl.add_exn table ~key:"big"
    ~data:
      ( module Meta.MakeInterpreter (Arith.MakeArith (Arith.BigStep))
      : Meta.Interpreter );
  Hashtbl.add_exn table ~key:"untyped"
    ~data:(module Meta.MakeInterpreter (Untyped.Untyped) : Meta.Interpreter);
  table

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
        let table = make_lang_table in
        let m = Hashtbl.find_exn table evaluator in
        match filename with
        | Some file -> interpret_file m file
        | None -> repl m)

let () = Command.run ~version:"1.0" command
