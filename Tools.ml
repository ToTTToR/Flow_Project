open Graph

let clone_nodes gr = n_fold gr new_node empty_graph
 
let gmap gr f = e_fold gr (fun ngr id1 id2 lbl -> new_arc ngr id1 id2 (f lbl)) (clone_nodes gr)

let add_arc gr id1 id2 n = match (find_arc gr id1 id2) with
  | None -> new_arc gr id1 id2 n 
  | Some len -> new_arc gr id1 id2 (n+len)

type path = id list
(*Create a path between to nodes, return None if no path available*)
let find_path gr forbidden id1 id2 =  
  let rec create_path forbid = function
    | [] -> failwith "No valid node to go"
    | (id,len)::rest -> if id=id2 then [id] else if List.exists (fun x -> x=id) forbid then create_path forbid rest
    else try 
      id::(create_path (id::forbid) (out_arcs gr id))
    with e -> create_path forbid rest
  in
  try Some (id1 :: (create_path (id1::forbidden) (out_arcs gr id1)))
  with e -> None