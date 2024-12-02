open Graph
open Tools

let children graph node = e_fold graph (fun l arc -> if arc.src = node then arc.tgt :: l else l) []

let rec find_path graph forbidden src tgt =
  if src = tgt
  then Some [src]
  else
    match children graph src with
      | [] -> None
      | c -> List.fold_left (
      fun ok node -> if ok then find_path graph (src :: forbidden) node tgt else None
      ) true c
