open Graph

type path = id list

let children graph node = e_fold graph (fun l arc -> if arc.src = node then arc.tgt :: l else l) []

let rec find_path graph forbidden src tgt =
  if src = tgt
  then Some [src]
  else (
    let rec dfs = function
      | [] -> None
      | child :: rest -> (
          match find_path graph (src :: forbidden) child tgt with
          | None -> dfs rest
          | Some path -> Some (src :: path)
        ) in
    dfs (List.filter (fun e -> not (List.exists (fun f -> f = e) forbidden)) (children graph src))
  )
