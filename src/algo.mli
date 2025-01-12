open Graph

type path = id list

type path_finder = int graph -> id -> id -> path option

val base_ff: path_finder -> int graph -> id -> id -> int graph

val ff: int graph -> id -> id -> int graph

val ff2: int graph -> id -> id -> int graph
