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
    | [] -> failwith "No valid path"
    | (id,len)::rest -> if id=id2 then [id] else if (List.exists (fun x -> x=id) forbid) || len=0 then create_path forbid rest
    else try 
      id::(create_path (id::forbid) (out_arcs gr id))
    with e -> create_path forbid rest
  in
  try Some (id1 :: (create_path (id1::forbidden) (out_arcs gr id1)))
  with e -> None

let rec find_min_flow gr m = function
  | [] -> m
  | [_] -> m
  | id1::id2::rest -> match find_arc gr id1 id2 with
    | None -> failwith "arc non existante"
    | Some len -> find_min_flow gr (min m len) (id2::rest)  

let graphe_ecart gr path =
  let min_flow = find_min_flow gr 9999 path in 
  let rec helper gr = function
    | [] -> gr
    | [_] -> gr
    | id1::id2::rest -> let gr1=(add_arc gr id1 id2 (-min_flow)) in
    let gr2=(add_arc gr1 id2 id1 min_flow) in helper gr2 (id2::rest)
  in
  helper gr path 

(*let ford_fulkerson gr id1 id2=
  let gr1=gmap gr int_of_string in (*Initialisation*)
  try
    let first_path gr id1 id2=find_path gr [] id1 id2 in
    
  with e -> failwith "Pas de chemin trouvé entre ces deux points, veuillez choisir des autres noeuds"*)