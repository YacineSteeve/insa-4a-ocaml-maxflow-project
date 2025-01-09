open Graph

type path = id list

type alias = {
  id: id;
  name: string;
}

exception Format_error of string

val read_file : string -> int graph * alias list * id * id

val list_append_uniq : 'a list -> 'a list -> 'a list

val build_wishes_graph : id -> id -> (id * id list) list -> int graph

val format_result_graph : int graph -> id -> id -> string graph

val compute_result_graph : string -> string graph * alias list

val get_result_filenames : string -> string * string

val grant_wishes: string -> unit
