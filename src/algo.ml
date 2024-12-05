open Graph
open Tools

type path = id list

let children graph node = List.fold_left (fun l arc -> if arc.lbl != 0 then arc.tgt :: l else l) [] (out_arcs graph node)

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

let init_flow_graph graph = e_fold graph (fun g arc -> add_arc g arc.src arc.tgt 0) (clone_nodes graph)

let rec get_flow_increment graph = function
  | []  | [_] -> max_int
  | src :: tgt :: rest -> (
      match find_arc graph src tgt with
      | None -> raise (Graph_error "invalid path")
      | Some arc -> min arc.lbl (get_flow_increment graph (tgt :: rest))
    )

let rec update_flow_graph graph increment = function
  | []  | [_] -> graph
  | src :: tgt :: rest -> update_flow_graph (add_arc graph src tgt increment) increment (tgt :: rest)

let rec update_residual_graph graph increment = function
  | []  | [_] -> graph
  | src :: tgt :: rest ->
      update_residual_graph (add_arc (add_arc graph src tgt (-increment)) tgt src increment) increment (tgt :: rest)

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
