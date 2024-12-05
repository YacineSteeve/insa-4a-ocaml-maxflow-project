open Graph

type path = id list

val children: int graph -> id -> id list

val find_path: int graph -> id list -> id -> id -> path option

val init_flow_graph: int graph -> int graph

val get_flow_increment: int graph -> path -> int

val update_flow_graph: int graph -> int -> path -> int graph

val update_residual_graph: int graph -> int -> path -> int graph

val get_final_graph: int graph -> int graph -> int graph

val ff: int graph -> id -> id -> int graph
