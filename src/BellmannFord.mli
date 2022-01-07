open Graph
open Tools
open Printf

type label = (id * int  * id)

val bellmanford: int graph -> id -> id -> id list option

val print_path : id list option -> unit