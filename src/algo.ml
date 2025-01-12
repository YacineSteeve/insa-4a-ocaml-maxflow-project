open Graph
open Tools

type path = id list

type path_finder = int graph -> id -> id -> path option

(*
 * Arcs with label 0 are ignored for this specific Ford-Fulkerson implementation
 * (Here "no arc" then means "no arc or no arc with non null label")
 *
 * int graph -> id -> id list
 *)
let children graph node = List.fold_left (fun l arc -> if arc.lbl != 0 then arc.tgt :: l else l) [] (out_arcs graph node)

(*
 * Uses a Depth First Search algorithm
 *
 * path_finder
 *)
let find_any_path graph src tgt =
  let rec search graph forbidden src tgt =
    if src = tgt
    then Some [src]
    else (
      let rec loop_children = function
        | [] -> None
        | child :: rest -> (
            match search graph (src :: forbidden) child tgt with
            | None -> loop_children rest
            | Some path -> Some (src :: path)
          ) in
      loop_children (List.filter (fun e -> not (List.exists (fun f -> f = e) forbidden)) (children graph src))
    ) in
  search graph [] src tgt

(*
 * Uses Bellman-Ford algorithm
 *
 * path_finder
 *)
let find_shortest_path graph src tgt =
  if src = tgt
  then Some [src]
  else (
    let nodes_count = n_fold graph (fun c _ -> c + 1) 0 in
    let costs = Hashtbl.create nodes_count
    and predecessors  = Hashtbl.create nodes_count in
    n_iter graph (fun node -> Hashtbl.replace costs node max_int) ;
    Hashtbl.replace costs src 0 ;
    let rec loop_nodes n =
      if n < nodes_count
      then (
        e_iter graph (fun arc ->
            let new_cost = arc.lbl + (Hashtbl.find costs arc.src) in
            if new_cost < (Hashtbl.find costs arc.tgt) then
              Hashtbl.replace costs arc.tgt new_cost ;
            Hashtbl.replace predecessors arc.tgt arc.src
          ) ;
        loop_nodes (n + 1)
      )
    in
    let _ = loop_nodes 1 in
    let rec build_path acc child =
      if child = src
      then Some acc
      else (
        try
          let parent = Hashtbl.find predecessors child in
          build_path (parent :: acc) parent
        with Not_found -> None
      )
    in
    Seq.iter (fun (k, v) -> Printf.printf "%d -> %d \n" k v) (Hashtbl.to_seq costs) ;
    match build_path [tgt] tgt with
    | None -> None
    | Some path ->
      List.iter (Printf.printf "%d ") path ;
      Printf.printf "\n" ; Some path
  )

(*
 * Get the minimum flow along a path, which corresponds to the flow increment
 *
 * int graph -> path -> int
 *)
let rec get_flow_increment graph = function
  | []  | [_] -> max_int
  | src :: tgt :: rest -> (
      match find_arc graph src tgt with
      | None -> raise (Graph_error "invalid path")
      | Some arc -> min arc.lbl (get_flow_increment graph (tgt :: rest))
    )

(*
 * int graph -> int -> path -> int graph
 *)
let rec update_residual_graph graph increment = function
  | []  | [_] -> graph
  | src :: tgt :: rest ->
    (* Subtract the increment for the normal arc direction, and add it for the inverse arc direction *)
    update_residual_graph (add_arc (add_arc graph src tgt (-increment)) tgt src increment) increment (tgt :: rest)

(*
 * Basically removes non relevant arcs
 *
 * int graph -> int graph -> int graph
 *)
let get_final_graph initial_graph residual_graph =
  e_fold initial_graph (
    fun g arc -> match find_arc residual_graph arc.tgt arc.src with
      | None -> g
      | Some residual_arc -> new_arc g { arc with lbl = residual_arc.lbl }
  ) (clone_nodes initial_graph)

(* Runs the Ford-Fulkerson algorithm with a given path finding function *)
let base_ff find_path graph src tgt =
  let rec loop residual_graph = (
    match find_path residual_graph src tgt with
    | None -> residual_graph
    | Some path -> (
        let flow_increment = get_flow_increment residual_graph path in
        loop (update_residual_graph residual_graph flow_increment path)
      )
  ) in
  get_final_graph graph (loop graph)

(* FF with any path *)
let ff = base_ff find_any_path

(* FF with the shortest path *)
let ff2 = base_ff find_shortest_path
