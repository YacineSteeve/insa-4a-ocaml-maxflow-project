open Graph
open Gfile
open Algo

type path = id list

type alias = {
  id: id;
  name: string;
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
      | _ -> raise (Format_error "Invalid wishes file format")
    with End_of_file -> graph, id_start, existing_aliases, (List.map (fun w -> w.id) existing_wishes)
  in
  let initial_graph = new_node empty_graph source_id in
  let inter_graph, sink_id, all_aliases, wishes_ids = loop initial_graph (source_id + 1) [] [] in
  let final_graph = List.fold_left (
      fun g wish_id -> new_arc g { src = wish_id; tgt = sink_id; lbl = 1 }
    ) (new_node inter_graph sink_id) wishes_ids
  in
  close_in file ;
  final_graph, all_aliases, source_id, sink_id

let rec list_append_uniq l = function
  | [] -> l
  | x :: rest -> list_append_uniq (if List.mem x l then l else x :: l) rest

let build_wishes_graph source_id sink_id wisher_wishes_assocs =
  let initial_graph = new_node empty_graph source_id in
  let rec loop_assocs graph wishes_ids_acc = function
    | [] -> graph, wishes_ids_acc
    | (wisher_id, wishes_ids) :: rest -> (
      let graph_with_wisher = try new_node graph wisher_id with Graph_error _ -> graph in
      let updated_graph = List.fold_left (
        fun g wish_id -> (
          let g_with_wish = try new_node g wish_id with Graph_error _ -> g in
          new_arc g_with_wish { src = wisher_id; tgt = wish_id; lbl = 1 }
        )
      ) (new_arc graph_with_wisher { src = source_id; tgt = wisher_id; lbl = 1 }) wishes_ids in
      loop_assocs updated_graph (list_append_uniq wishes_ids_acc wishes_ids) rest
    ) in
  let partial_graph, wishes_ids = loop_assocs initial_graph [] wisher_wishes_assocs in
  List.fold_left (
    fun graph wish_id -> new_arc graph { src = wish_id; tgt = sink_id; lbl = 1 }
  ) (new_node partial_graph sink_id) wishes_ids

let format_result_graph graph source_id sink_id = e_fold graph (
    fun g arc -> if arc.src = source_id || arc.tgt = sink_id
      then g
      else (
        let g_with_src = try new_node g arc.src with _ -> g in
        let g_with_src_tgt = try new_node g_with_src arc.tgt with _ -> g_with_src in
        try new_arc g_with_src_tgt { arc with lbl = "has" } with _ -> g_with_src_tgt
      )
  ) empty_graph

let compute_result_graph wishes_file =
  let graph, aliases, source_id, sink_id = read_file wishes_file in
  let solved_graph = ff graph source_id sink_id in
  let result_graph = format_result_graph solved_graph source_id sink_id in
  (result_graph, aliases)

let get_result_filenames wishes_filename =
  let result_filename = match String.split_on_char '.' wishes_filename with
    | name :: _ -> name ^ "_result"
    | _ -> "wishes_result" in
  (result_filename ^ ".dot", result_filename ^ ".svg")

let grant_wishes wishes_file =
  let result_dotfile_name, result_svgfile_name = get_result_filenames wishes_file
  and result_graph, aliases = compute_result_graph wishes_file in

  map_export (
    fun node -> let node_alias = List.find (fun alias -> alias.id = node) aliases in "\"" ^ node_alias.name ^ "\""
  ) result_dotfile_name result_graph ;

  let exit_code = Sys.command (
      "dot -Tsvg " ^ result_dotfile_name ^ " > " ^ result_svgfile_name ^ " && rm " ^ result_dotfile_name
    ) in

  Printf.printf "%s" (
    if exit_code = 0
    then ("Results generated successfully (see " ^ result_svgfile_name ^ ")\n\n")
    else "Something went wrong\n\n"
  ) ;

  ()
