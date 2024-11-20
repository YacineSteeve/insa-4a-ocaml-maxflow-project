(*

  You do not need to read this file for the project. Read graph.mli instead.

  You may read it though, out of curiosity.


























*)


type id = int

type 'a arc =
  { src: id ;
    tgt: id ;
    lbl: 'a }

(* A graph is just a list of pairs: a node & its outgoing arcs. *)
type 'a graph = (id * 'a arc list) list

exception Graph_error of string

let empty_graph = []

let node_exists gr id = List.mem_assoc id gr

let out_arcs gr id =
  try List.assoc id gr
  with Not_found -> raise (Graph_error ("Node " ^ string_of_int id ^ " does not exist in this graph."))

let find_arc gr id1 id2 = List.find_opt (fun arc -> arc.tgt = id2) (out_arcs gr id1)

let new_node gr id =
  if node_exists gr id then raise (Graph_error ("Node " ^ string_of_int id ^ " already exists in the graph."))
  else (id, []) :: gr

let new_arc gr arc =

  (* Existing out-arcs *)
  let outa = out_arcs gr arc.src in

  (* Update out-arcs. *)
  let outb = arc :: List.filter (fun a -> a.tgt <> arc.tgt) outa in

  (* Replace out-arcs in the graph. *)
  let gr2 = List.remove_assoc arc.src gr in
  (arc.src, outb) :: gr2

let n_iter gr f = List.iter (fun (id, _) -> f id) gr

let n_iter_sorted gr f = n_iter (List.sort compare gr) f

let n_fold gr f acu = List.fold_left (fun acu (id, _) -> f acu id) acu gr

let e_iter gr f = List.iter (fun (_, out) -> List.iter f out) gr

let e_fold gr f acu = List.fold_left (fun acu (_, out) -> List.fold_left (fun acu arc -> f acu arc) acu out) acu gr

