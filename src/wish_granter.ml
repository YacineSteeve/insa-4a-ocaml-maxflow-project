open Wfile
open Gfile
open Tools

let () =
  if Array.length Sys.argv <> 2 && Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s wishes_file [outfile]\n\n%s%!" Sys.argv.(0)
        ("    🟄  wishes_file  : input file containing the wishes of the job seekers.\n\n") ;

      exit 0
    end ;

    let wishes_file = Sys.argv.(1) in

    let graph, aliases, source_id, sink_id = read_file wishes_file in
    export "test.dot" (gmap graph string_of_int) ;
    Printf.printf "Source -> %d\nSink -> %d\n\n" source_id sink_id ;
    List.iter (fun a -> Printf.printf "%s -> %d\n" a.name a.id) aliases ;
    ()
