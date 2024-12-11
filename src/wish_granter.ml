open Wfile

let () =
  if Array.length Sys.argv <> 2 && Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s wishes_file [outfile]\n\n%s%!" Sys.argv.(0)
        ("    🟄  wishes_file  : input file containing the wishes of the job seekers.\n\n") ;

      exit 0
    end ;

    let wishes_file = Sys.argv.(1) in

    grant_wishes wishes_file ;

    ()
