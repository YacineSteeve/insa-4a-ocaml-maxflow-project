open Graph
open Gfile
open Algo
open Tools

type path = id list

type alias = {
  id: id;
  name: string;
}

exception Format_error of string

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

(* Append a list to l, by ignoring elements already in l *)
let rec list_append_uniq l = function
  | [] -> l
  | x :: rest -> list_append_uniq (if List.mem x l then l else x :: l) rest

(* Connect source to wishers, wishers to wishes, and wishes to sink
 * Labels are set to 1 to make the Ford-Fulkerson algorithm act as a bipartite matching one
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

let format_result_graph graph source_id sink_id = e_fold graph (
    fun g arc -> if arc.src = source_id || arc.tgt = sink_id (* Removing source and sink in final graph *)
      then g
      else (
        let g_with_src = try new_node g arc.src with _ -> g in
        let g_with_src_tgt = try new_node g_with_src arc.tgt with _ -> g_with_src in
        try new_arc g_with_src_tgt { arc with lbl = "has" } with _ -> g_with_src_tgt (* Use of a more relevant label *)
      )
  ) empty_graph

let in_arcs graph node = e_fold graph (fun l arc -> if arc.tgt = node then arc :: l else l) []

let clean_result_graph graph = e_fold graph (
    fun g arc -> if List.length (out_arcs graph arc.src) > 1 && List.length (in_arcs graph arc.tgt) > 1
      then g
      else new_arc g arc
  ) (clone_nodes graph)

let compute_result_graph wishes_file_path =
  let source_id, sink_id, aliases, wisher_wishes_assocs = parse_wishes_file wishes_file_path in
  let graph = build_wishes_graph source_id sink_id wisher_wishes_assocs in
  let solved_graph = ff graph source_id sink_id in
  let _result_graph = format_result_graph solved_graph source_id sink_id in
  (solved_graph, aliases)

let get_result_filenames wishes_filename =
  let result_filename = match String.split_on_char '.' wishes_filename with
    | name :: _ -> name ^ "_result"
    | _ -> "wishes_result" in
  (result_filename ^ ".dot", result_filename ^ ".svg")

let grant_wishes wishes_file =
  let result_dotfile_name, result_svgfile_name = get_result_filenames wishes_file
  and result_graph, aliases = compute_result_graph wishes_file in

  (* Generation of the dotfile, putting node values between quotes to prevent parsing errors due to spaces *)
  map_export (
    fun node -> let node_alias = try List.find (fun alias -> alias.id = node) aliases with _ -> { id = 1; name =  string_of_int node } in "\"" ^ node_alias.name ^ "\""
  ) result_dotfile_name (gmap result_graph string_of_int) ;

  let exit_code = Sys.command (
      "dot -Tsvg " ^ result_dotfile_name ^ " > " ^ result_svgfile_name
    ) in

  Printf.printf "%s" (
    if exit_code = 0
    then ("Results generated successfully (see " ^ result_svgfile_name ^ ")\n\n")
    else "Something went wrong\n\n"
  ) ;

  ()
