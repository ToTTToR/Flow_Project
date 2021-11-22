open Graph
open Printf

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
    | [_] -> printf "Je suis appelé 2\n";gr
    | id1::id2::rest -> printf "Je suis appelé 1\n";let gr1=(add_arc gr id1 id2 (-min_flow)) in
    let gr2=(add_arc gr1 id2 id1 min_flow) in helper gr2 (id2::rest)
  in
  helper gr path 

let print_path path = List.iter (printf "%d ") path;printf("\n")

let make_real_path = function
  | None -> []
  | Some x -> x

let test_1 gr id1 id2 =
  let gr1=gmap gr int_of_string in
  let pat1=make_real_path (find_path gr1 [] id1 id2) in
  let gr2=graphe_ecart gr1 pat1 in
  let pat2=make_real_path (find_path gr2 [] id1 id2) in
  let gr3=gmap (graphe_ecart gr2 pat2) string_of_int in
  print_path pat1;
  print_path pat2;
  gr3

let ford_fulkerson gr id1 id2=
  let gr1=gmap gr int_of_string in (*Initialisation*)
  let rec helper gra = (*Tant qu'on trouve un chemin, on fait le graphe d'écart associé*)
    match (find_path gra [] id1 id2) with 
      | None -> gra
      | Some pat -> (print_path pat);helper (graphe_ecart gra pat)
  in gmap (helper gr1) string_of_int