open Graph
open Tools
open Printf

type label = (id * int  * id)

val print_list_label: label list -> unit
val replace : label list -> id -> label -> label list
val find_label : label list -> id -> label
val bellmanford: int graph -> id -> id -> id list option
val print_path : id list option -> unit