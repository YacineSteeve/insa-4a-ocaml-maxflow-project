open Graph

type path = id list

val children: int graph -> id -> id list

val find_path: int graph -> id list -> id -> id -> path option
