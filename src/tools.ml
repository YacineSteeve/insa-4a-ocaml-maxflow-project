open Graph

let clone_nodes graph = n_fold graph new_node empty_graph

let gmap graph f = e_fold graph (
    fun out_g curr_arc -> new_arc out_g { curr_arc with lbl = f curr_arc.lbl }
  ) (clone_nodes graph)

let add_arc graph id1 id2 value =
  new_arc graph {
    src = id1;
    tgt = id2;
    lbl = match (find_arc graph id1 id2) with
      | None -> value
      | Some existing_arc -> existing_arc.lbl + value
  }
