open Graph

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a-> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph 

type path = id list

val find_path: int graph -> id list -> id -> id -> path option
val graphe_ecart: int graph -> path -> int graph
val ford_fulkerson: string graph -> int -> int -> string graph

val test_1: string graph -> int -> int -> string graph