open Graph
open Tools

type path = id list

val find_path: int graph -> id list -> id -> id -> path option
val graphe_ecart: int graph -> path -> int graph
val ford_fulkerson: string graph -> int -> int -> string graph