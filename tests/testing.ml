let eval interpreter fixture =
  let module I = (val interpreter : Meta.Interpreter) in
  fixture |> I.read ~origin:fixture |> I.eval |> I.print
