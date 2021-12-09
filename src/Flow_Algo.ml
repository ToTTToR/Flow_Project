open Graph
open Printf
open Tools
open Dijkstra
open BellmannFord

type path = id list

(*Create a path between to nodes, return None if no path available*)
let find_path gr forbidden id1 id2 =  
  let rec create_path forbid = function
    (*If node has no child, or if all child has already been visited, not a valid path to make*)
    | [] -> failwith "No valid path" 
    (*Check if next node has already been visited, or if out_arc to it is null.*)
    | (id,len)::rest -> if List.mem id forbid || len=0 then create_path forbid rest 
      else if id=id2 then [id] (*Destination found!*)
      else try 
          id::(create_path (id::forbid) (out_arcs gr id))
        with e -> create_path forbid rest (*Make another path if previous not valid*)
  in
  try Some (id1 :: (create_path (id1::forbidden) (out_arcs gr id1)))
  with e -> None

(*Find minimum flow value of given path to increment*)
let rec find_min_flow gr m = function
  | [] -> m
  | [_] -> m
  | id1::id2::rest -> match find_arc gr id1 id2 with
    | None -> failwith "arc non existante"
    | Some len -> if len=0 then failwith "arc pas possible a traverser" (*This error should not occur though...*)
      else find_min_flow gr (min m len) (id2::rest)  

(*Make "graphe d'ecart" after given path has been found*)
let graphe_ecart gr path =
  printf "Creation graphe d'ecart de flot\n\n%!";
  let min_flow = find_min_flow gr Int.max_int path in (*Should initially compare to infinity*)
  let rec helper gr = function
    | [] -> gr
    | [_] -> gr
    (*For every arc pointing forward to destination, decrease flow*)
    | id1::id2::rest -> let gr1=(add_arc gr id1 id2 (-min_flow)) in
      (*For every arc pointing forward to origin, increase flow*)
      let gr2=(add_arc gr1 id2 id1 min_flow) in helper gr2 (id2::rest)
  in
  helper gr path 

let print_path path = (printf "Chemin associé : ");List.iter (printf "%d ") path;printf("\n")

let make_real_path = function
  | None -> []
  | Some x -> x

(*Show graph with flow/capacity as label*)
let pretty_print_graph gr gr_ecart =
  e_fold gr (fun ngr id1 id2 cap -> 
      match find_arc gr_ecart id2 id1 with
      | None -> new_arc ngr id1 id2 ("0/"^cap)
      | Some x -> new_arc ngr id1 id2 (x^"/"^cap)
    ) (clone_nodes gr)

(*Solve flow max problem of graph*)
let ford_fulkerson gr id1 id2=
  let gr1=gmap gr int_of_string in
  let rec helper gra = (*Tant qu'on trouve un chemin, on fait le graphe d'écart associé*)
    match (find_path gra [] id1 id2) with 
    | None -> gra (*Si pas de chemin trouvé, renvoie notre dernier graphe d'écart*)
    | Some pat -> (print_path pat);helper (graphe_ecart gra pat)
  in 
  let gr2=gmap (helper gr1) string_of_int in
  pretty_print_graph gr gr2

let make_graph_cost_ecart base_graph graph_flow_ecart graph_cost =
  e_fold base_graph (fun new_g id1 id2 cost -> 
    let cost_arc = match find_arc graph_cost id1 id2 with
      | None -> 0
      | Some x -> x in
    match find_arc graph_flow_ecart id1 id2 with
    | None -> new_g
    | Some y -> if y==0 then new_arc new_g id2 id1 (-cost_arc)  else
     match find_arc graph_flow_ecart id2 id1 with
      | None -> new_arc new_g id1 id2 cost_arc
      | _ -> new_arc (new_arc new_g id2 id1 (-cost_arc)) id1 id2 cost_arc
  ) (clone_nodes base_graph)

let print_graph gr = e_iter gr (fun id1 id2 cost -> printf "Arc de %d à %d de cout %d\n%!" id1 id2 cost)


let busacker_gowen (graph_flow_s,graph_cost_s) id1 id2=
  let graph_flow=gmap graph_flow_s int_of_string in
  let graph_cost=gmap graph_cost_s int_of_string in
  let rec helper gr_fl_ecart gr_cost_ecart =
    match bellmanford gr_cost_ecart id1 id2 with
    | None -> printf "Plus de chemin trouvé\n%!";print_graph gr_fl_ecart; gr_fl_ecart
    | Some path -> print_path path;
    let next_gr_fl_ecart=graphe_ecart gr_fl_ecart path in
    printf "Prochain graphe d'écart de flot : \n%!";print_graph next_gr_fl_ecart;
    let next_gr_cost_ecart=make_graph_cost_ecart graph_flow next_gr_fl_ecart graph_cost in
    printf "Prochain graphe d'écart de cout : \n%!";print_graph next_gr_cost_ecart;
    helper next_gr_fl_ecart next_gr_cost_ecart
  in
  let gr2=gmap (helper graph_flow graph_cost) string_of_int in
  pretty_print_graph graph_flow_s gr2