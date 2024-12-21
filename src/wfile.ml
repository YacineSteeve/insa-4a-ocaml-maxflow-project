open Graph
open Gfile
open Algo
open Tools

type path = id list

type alias = {
  id: id;
  name: string;
  weight: int;
}

exception Format_error of string

let read_file wish_file_path =
  let file = open_in wish_file_path in
  let source_id = 0 in
  let rec loop graph id_start existing_aliases existing_wishes =
    try
      let line = input_line file in
      let next_id = ref id_start in
      match String.split_on_char ':' line with
        | first :: second :: [] -> (
          let wisher = {
            id = !next_id;
            name = String.trim first;
            weight = 0;
          }
          and wishes_raw = next_id := !next_id + 1 ; String.split_on_char ';' second in
          let use_index = not (String.contains line '=') || (
            if List.for_all (fun wish_raw -> String.contains wish_raw '=') wishes_raw
            then false
            else raise (Format_error "Invalid wish format")
          )
          in
          let wishes = List.mapi (
            fun index wish_raw -> (
              let wish_with_preference = String.split_on_char '=' wish_raw in
              try
                let wish_name = String.trim (List.hd wish_with_preference) in
                {
                  name = wish_name;
                  id = (
                    try
                      let existing_wish = List.find (fun w -> w.name = wish_name) existing_wishes in
                      existing_wish.id
                    with Not_found -> next_id := !next_id + 1 ; !next_id - 1
                  );
                  weight = if use_index
                    then index + 1
                    else int_of_string (String.trim (List.nth wish_with_preference 1))
                }
              with _ -> raise (Format_error "Invalid wish format")
            )
          ) wishes_raw in
          let updated_graph = List.fold_left (
            fun g wish -> new_arc (
              if node_exists g wish.id then g else new_node g wish.id
            ) { src = wisher.id; tgt = wish.id; lbl = wish.weight }
          ) (new_arc (new_node graph wisher.id) { src = source_id; tgt = wisher.id; lbl = 0 }) wishes
          and all_wishes = List.append wishes existing_wishes in
          loop updated_graph !next_id (List.concat [[wisher]; wishes; existing_aliases]) all_wishes
        )
        | _ -> raise (Format_error "Invalid wishes file format")
    with End_of_file -> graph, id_start, existing_aliases, (List.map (fun w -> w.id) existing_wishes)
  in
    let initial_graph = new_node empty_graph source_id in
    let inter_graph, sink_id, all_aliases, wishes_ids = loop initial_graph (source_id + 1) [] [] in
    let final_graph = List.fold_left (
      fun g wish_id -> new_arc g { src = wish_id; tgt = sink_id; lbl = 0 }
    ) (new_node inter_graph sink_id) wishes_ids
  in
  close_in file ;
  final_graph, all_aliases, source_id, sink_id ;;

let grant_wishes wishes_file =
  let graph, aliases, source_id, sink_id = read_file wishes_file in
  map_export string_of_int "graph.dot" (gmap graph string_of_int) ; (* debug *)
  let solved_graph = ff graph source_id sink_id in

  let cleaned_graph = e_fold solved_graph (
    fun g arc -> if arc.src = source_id || arc.tgt = sink_id
      then g
      else (
        let g_with_src = try new_node g arc.src with _ -> g in
        let g_with_src_tgt = try new_node g_with_src arc.tgt with _ -> g_with_src in
        try new_arc g_with_src_tgt { arc with lbl = "has" } with _ -> g_with_src_tgt
      )
  ) empty_graph in

  let result_filename = match String.split_on_char '.' wishes_file with
    | name :: _ -> name ^ "_result"
    | _ -> "wishes_result" in

  let result_dotfile_name = result_filename ^ ".dot"
  and result_svgfile_name = result_filename ^ ".svg" in

  map_export (
    fun node -> let node_alias = List.find (fun alias -> alias.id = node) aliases in "\"" ^ node_alias.name ^ "\""
  ) result_dotfile_name cleaned_graph ;

  let exit_code = Sys.command ("dot -Tsvg " ^ result_dotfile_name ^ " > " ^ result_svgfile_name) in

  Printf.printf "%s" (
    if exit_code = 0
    then ("Results generated successfully (see " ^ result_svgfile_name ^ ")\n\n")
    else "Something went wrong\n\n"
  ) ;

  ()
