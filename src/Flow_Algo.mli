open Graph
open Tools

type path = id list

val find_path: int graph -> id list -> id -> id -> path option
val print_path: id list -> unit
val graphe_ecart: int graph -> path -> int graph
val ford_fulkerson: string graph -> id -> id -> string graph