(*Algo inutile...*)

open Set
open Graph
open Tools
open Printf

type label = (id * int * bool * id)

module Heap : Set.S

val print_list_label: Heap.elt list -> unit
val replace : label list -> id -> label -> label list
val find_label : label list -> id -> label
val dijkstra: string graph -> id -> id -> id list option