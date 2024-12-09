open Graph

type path = id list

type alias = {
  id: id;
  name: string;
}

val read_file : string -> int graph * alias list * id * id
