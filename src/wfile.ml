open Graph
open Gfile
open Algo

type alias = {
  id: id;
  name: string;
}

exception Format_error of string

(*
 * Extract data from the provided file and format it for future usage in the program
 *
 * string -> id * id * alias list * (id * id list) list
 *)
let parse_wishes_file wishes_file_path =
  let file = open_in wishes_file_path in
  let rec loop_lines start_id aliases wisher_wishes_assocs existing_wishes = (
    try
      let line = input_line file in
      let next_id = ref start_id in (* To keep track of state of the ids sequence for the current line *)
      match String.split_on_char ':' line with
      | first :: second :: [] -> (
          let wisher = {
            id = !next_id; (* The wisher takes the next available id *)
            name = String.trim first
          }
          and wishes_raw = next_id := !next_id + 1 ; String.split_on_char ';' second in
          let wishes = List.map (
              fun wish -> (
                  let wish_name = String.trim wish in
                  {
                    name = wish_name;
                    id = (
                      (* Each wish is associated to:
                       * either the existing id if it was been seen before
                       * or the next available id
                      *)
                      try
                        let existing_wish = List.find (fun w -> w.name = wish_name) existing_wishes in
                        existing_wish.id
                      with Not_found -> next_id := !next_id + 1 ; !next_id - 1 (* Pre increment *)
                    )
                  }
                )
            ) wishes_raw in
          let updated_aliases = (List.concat [[wisher]; wishes; aliases])
          and updated_wisher_wishes_assocs = (wisher.id, List.map (fun w -> w.id) wishes) :: wisher_wishes_assocs
          and updated_wishes = List.append wishes existing_wishes in
          loop_lines !next_id updated_aliases updated_wisher_wishes_assocs updated_wishes
        )
      | _ -> raise (Format_error "Invalid wishes file format")
    with End_of_file -> start_id, aliases, wisher_wishes_assocs (* sink_id being the last value of start_id *)
  )
  and source_id = 0 in
  let sink_id, aliases, wisher_wishes_assocs = loop_lines (source_id + 1) [] [] [] in
  close_in file ;
  (source_id, sink_id, aliases, wisher_wishes_assocs)

(*
 * Append a list to l, by ignoring elements already in l
 *
 * 'a list -> 'a list -> 'a list
 *)
let rec list_append_uniq l = function
  | [] -> l
  | x :: rest -> list_append_uniq (if List.mem x l then l else x :: l) rest

(*
 * Connect source to wishers, wishers to wishes, and wishes to sink
 * Labels are set to 1 to make the Ford-Fulkerson algorithm act as a bipartite matching one
 *
 * id -> id -> (id * id list) list -> int graph
 *)
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

(*
 * int graph -> id -> id -> string graph
 *)
let clean_solved_graph graph source_id sink_id = e_fold graph (
    fun g arc -> if arc.src = source_id || arc.tgt = sink_id || arc.lbl = 0 (* Removing source, sink, and null arcs from final graph *)
      then g
      else (
        let g_with_src = try new_node g arc.src with _ -> g in
        let g_with_src_tgt = try new_node g_with_src arc.tgt with _ -> g_with_src in
        try new_arc g_with_src_tgt {arc with lbl = "has" } with _ -> g_with_src_tgt
      )
  ) empty_graph

(*
 * string -> string graph * alias list
 *)
let compute_result_graph wishes_file_path =
  let source_id, sink_id, aliases, wisher_wishes_assocs = parse_wishes_file wishes_file_path in
  let graph = build_wishes_graph source_id sink_id wisher_wishes_assocs in
  let solved_graph = ff graph source_id sink_id in (* Use of Ford-Fulkerson *)
  let result_graph = clean_solved_graph solved_graph source_id sink_id in
  (result_graph, aliases)

(*
 * string -> string * string
 *)
let get_result_filenames wishes_filename =
  let result_filename = match String.split_on_char '.' wishes_filename with
    | name :: _ -> name ^ "_result"
    | _ -> "wishes_result" in
  (result_filename ^ ".dot", result_filename ^ ".svg")

let grant_wishes wishes_file =
  let result_dotfile_name, result_svgfile_name = get_result_filenames wishes_file
  and result_graph, aliases = compute_result_graph wishes_file in

  (* Generation of the dotfile
   * - remapping node ids to their actual display names
   * - putting names between quotes to prevent dotfile parsing errors due to spaces
  *)
  map_export (
    fun node -> (
        try
          let node_alias = List.find (fun alias -> alias.id = node) aliases in "\"" ^ node_alias.name ^ "\""
        with _ -> "Unknown"
      )
  ) result_dotfile_name result_graph ;

  let exit_code = Sys.command (
      "dot -Tsvg " ^ result_dotfile_name ^ " > " ^ result_svgfile_name
    ) in

  Printf.printf "%s" (
    if exit_code = 0
    then ("Results generated successfully (see " ^ result_svgfile_name ^ ")\n\n")
    else "Something went wrong\n\n"
  ) ;

  ()
