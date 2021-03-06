open Graph
open Printf

let clone_nodes gr = n_fold gr new_node empty_graph

let gmap gr f = e_fold gr (fun ngr id1 id2 lbl -> new_arc ngr id1 id2 (f lbl)) (clone_nodes gr)

let add_arc gr id1 id2 n = match (find_arc gr id1 id2) with
  | None -> new_arc gr id1 id2 n 
  | Some len -> new_arc gr id1 id2 (n+len)