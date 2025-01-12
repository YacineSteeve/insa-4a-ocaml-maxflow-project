open Graph
open Tools

type path = id list

(* Arcs with label 0 are ignored for this specific Ford-Fulkerson implementation
 * (Here "no arc" then means "no arc or no arc with non null label")
 *)
let children graph node = List.fold_left (fun l arc -> if arc.lbl != 0 then arc.tgt :: l else l) [] (out_arcs graph node)

(* DFS *)
let rec find_path graph forbidden src tgt =
  if src = tgt
  then Some [src]
  else (
    let rec loop_children = function
      | [] -> None
      | child :: rest -> (
          match find_path graph (src :: forbidden) child tgt with
          | None -> loop_children rest
          | Some path -> Some (src :: path)
        ) in
    loop_children (List.filter (fun e -> not (List.exists (fun f -> f = e) forbidden)) (children graph src))
  )

(* Create a copy of the graph with every labels at 0 *)
let init_flow_graph graph = e_fold graph (fun g arc -> add_arc g arc.src arc.tgt 0) (clone_nodes graph)

(* Get the minimum flow along a path, which corresponds to the flow increment *)
let rec get_flow_increment graph = function
  | []  | [_] -> max_int
  | src :: tgt :: rest -> (
      match find_arc graph src tgt with
      | None -> raise (Graph_error "invalid path")
      | Some arc -> min arc.lbl (get_flow_increment graph (tgt :: rest))
    )

(* Add the flow increment along a path *)
let rec update_flow_graph graph increment = function
  | []  | [_] -> graph
  | src :: tgt :: rest -> update_flow_graph (add_arc graph src tgt increment) increment (tgt :: rest)

let rec update_residual_graph graph increment = function
  | []  | [_] -> graph
  | src :: tgt :: rest ->
    (* Subtract the increment for the normal arc direction, and add it for the inverse arc direction *)
    update_residual_graph (add_arc (add_arc graph src tgt (-increment)) tgt src increment) increment (tgt :: rest)

(* Basically removes non relevant arcs *)
let get_final_graph initial_graph residual_graph =
  e_fold initial_graph (
    fun g arc -> match find_arc residual_graph arc.tgt arc.src with
      | None -> g
      | Some residual_arc -> new_arc g { arc with lbl = residual_arc.lbl }
  ) (clone_nodes initial_graph)

let ff graph src tgt =
  let rec loop residual_graph = (
    match find_path residual_graph [] src tgt with
    | None -> residual_graph
    | Some path -> (
        let flow_increment = get_flow_increment residual_graph path in
        loop (update_residual_graph residual_graph flow_increment path)
      )
  ) in
  get_final_graph graph (loop graph)
