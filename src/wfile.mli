open Graph

type path = id list

type alias = {
  id: id;
  name: string;
  weight: int;
}

exception Format_error of string

val read_file : string -> int graph * alias list * id * id

val grant_wishes: string -> unit
