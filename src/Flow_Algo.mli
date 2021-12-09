open Graph
open Tools

type path = id list

val find_path: int graph -> id list -> id -> id -> path option
val print_path: path -> unit
val graphe_ecart: int graph -> path -> int graph
val ford_fulkerson: string graph -> id -> id -> string graph
val busacker_gowen: string graph * string graph-> id -> id -> string graph