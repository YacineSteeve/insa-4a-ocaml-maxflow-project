open Graph

type path = id list

type alias = {
  id: id;
  name: string;
}

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
            name = String.trim first
          }
          and wishes_raw = next_id := !next_id + 1 ; String.split_on_char ';' second in
          let wishes = List.map (
            fun wish -> (
              let wish_name = String.trim wish in
              {
                name = wish_name;
                id = (
                  try
                    let existing_wish = List.find (fun w -> w.name = wish_name) existing_wishes in
                    existing_wish.id
                  with Not_found -> next_id := !next_id + 1 ; !next_id - 1
                )
              }
            )
          ) wishes_raw in
          let updated_graph = List.fold_left (
            fun g wish -> new_arc (
              if node_exists g wish.id then g else new_node g wish.id
            ) { src = wisher.id; tgt = wish.id; lbl = 1 }
          ) (new_arc (new_node graph wisher.id) { src = source_id; tgt = wisher.id; lbl = 1 }) wishes
          and all_wishes = List.append wishes existing_wishes in
          loop updated_graph !next_id (List.concat [[wisher]; wishes; existing_aliases]) all_wishes
        )
        | _ -> failwith "Invalid file format"
    with End_of_file -> graph, id_start, existing_aliases, (List.map (fun w -> w.id) existing_wishes)
  in
    let initial_graph = new_node empty_graph source_id in
    let inter_graph, sink_id, all_aliases, wishes_ids = loop initial_graph (source_id + 1) [] [] in
    let final_graph = List.fold_left (
      fun g wish_id -> new_arc g { src = wish_id; tgt = sink_id; lbl = 1 }
    ) (new_node inter_graph sink_id) wishes_ids
  in
  close_in file ;
  final_graph, all_aliases, source_id, sink_id ;;
