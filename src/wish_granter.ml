open Wfile

let () =
  if Array.length Sys.argv <> 2 then
    begin
      Printf.printf
        "\nUsage: %s wishes_file\n%s%!" Sys.argv.(0)
        ("    🟄  wishes_file  : input file containing the people and their list wishes.\n\n") ;

      exit 1
    end ;

  let wishes_file = Sys.argv.(1) in

  grant_wishes wishes_file ;

  ()
