open Graph

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (
fun out_g curr_arc -> new_arc out_g { curr_arc with lbl = f curr_arc.lbl }
) (clone_nodes gr)

let add_arc gr id1 id2 value =
  new_arc gr {
    src = id1;
    tgt = id2;
    lbl = match (find_arc gr id1 id2) with
      | None -> value
      | Some existing_arc -> existing_arc.lbl + value
  }
