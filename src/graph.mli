
(* Type of a directed graph in which arcs have labels of type 'a. 
 * This type is deliberately abstract. *)
type 'a graph

(* Each node has a unique identifier (a number). *)
type id = int

(* Type of an arc (edge) with labels of type 'a *)
type 'a arc =
  { (* Source *)
    src: id ;

    (* Target *)
    tgt: id ;

    (* Label *)
    lbl: 'a }

exception Graph_error of string


(**************  CONSTRUCTORS  **************)

(* The empty graph. *)
val empty_graph: 'a graph

(* Add a new node with the given identifier.
 * @raise Graph_error if the id already exists. *)
val new_node: 'a graph -> id -> 'a graph

(* new_arc gr arc : adds an arc. 
 * Both arc nodes must already exist in the graph.
 * If the arc already exists, its label is replaced by lbl. 
 * @raise Graph_error if node src or tgt does not exist in the graph. *)
val new_arc: 'a graph -> 'a arc -> 'a graph


(**************  GETTERS  *****************)

(* node_exists gr id  indicates if the node with identifier id exists in graph gr. *)
val node_exists: 'a graph -> id -> bool

(* Find the out_arcs of a node.
 * @raise Graph_error if the id is unknown in the graph. *)
val out_arcs: 'a graph -> id -> 'a arc list

(* find_arc gr id1 id2  finds an arc between id1 and id2. Returns None if the arc does not exist. 
 * @raise Graph_error if id1 is unknown. *)
val find_arc: 'a graph -> id -> id -> 'a arc option


(**************  COMBINATORS, ITERATORS  **************)

(* Iterate on all nodes, in no special order. *)
val n_iter: 'a graph -> (id -> unit) -> unit

(* Like n_iter, but the nodes are sorted. *)
val n_iter_sorted: 'a graph -> (id -> unit) -> unit

(* Fold on all (unsorted) nodes. You must remember what List.fold_left does. *)
val n_fold: 'a graph -> ('b -> id -> 'b) -> 'b -> 'b


(* Iter on all arcs (edges). *)
val e_iter: 'a graph -> ('a arc -> unit) -> unit

(* Fold on all arcs (edges). *)
val e_fold: 'a graph -> ('b -> 'a arc -> 'b) -> 'b -> 'b

