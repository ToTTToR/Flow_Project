open Graph

let clone_nodes gr = n_fold gr new_node empty_graph
<<<<<<< HEAD

let gmap gr f = let gr1=clone_nodes gr in
  e_fold gr (fun gr id1 id2 x -> new_arc gr1 id1 id2 (f x)) gr1

let add_arc gr id1 id2 len = new_arc gr id1 id2 len

=======
 
let gmap gr f = e_fold gr (fun ngr id1 id2 lbl -> new_arc ngr id1 id2 (f lbl)) (clone_nodes gr)

let add_arc gr id1 id2 len = assert false
>>>>>>> origin/Nailik
