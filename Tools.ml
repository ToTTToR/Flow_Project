open Graph

let clone_nodes gr = n_fold gr new_node empty_graph
 
let gmap gr f = e_fold gr f (clone_nodes gr)

let add_arc gr id1 id2 len = assert false