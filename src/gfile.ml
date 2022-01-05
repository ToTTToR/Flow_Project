open Graph
open Printf

type personne =
  {
    id : int;
    name : string;
    metierVoulueId : int list;
  }

type metier =
  {
    id : int;
    name : string;
  }

type matchPersonneWithMetier =
  {
    personnes : personne list; 
    metiers : metier list;
  }
let initList = {personnes=[];metiers=[]} 



type path = string

let iof = int_of_float
let foi = float_of_int

let index_i id = iof (sqrt (foi id *. 1.1))

let compute_x id = 20 + 180 * index_i id

let compute_y id =
  let i0 = index_i id in
  let delta = id - (i0 * i0 * 10 / 11) in
  let sgn = if delta mod 2 = 0 then -1 else 1 in

  300 + sgn * (delta / 2) * 100


let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  let _ = e_fold graph (fun count id1 id2 lbl -> fprintf ff "e %d %d %d %s\n" id1 id2 count lbl ; count + 1) 0 in

  fprintf ff "\n%% End of graph\n" ;

  close_out ff ;
  ()

(* Reads a line with a node. *)
let read_node graph line =
  try Scanf.sscanf line "n %f %f %d" (fun _ _ id -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e %d %d %_d %s@%%"
        (fun id1 id2 label -> new_arc (ensure (ensure graph id1) id2) id1 id2 label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let read_arc_bis (graph_flow,graph_cost) line =
  try Scanf.sscanf line "e %d %d %_d %s %s@%%"
        (fun id1 id2 label_flow label_cost -> (new_arc (ensure (ensure graph_flow id1) id2) id1 id2 label_flow),(new_arc (ensure (ensure graph_cost id1) id2) id1 id2 label_cost))
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"    

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          | 'n' -> read_node graph line
          | 'e' -> read_arc graph line

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop empty_graph in

  close_in infile ;
  final_graph


(* mise en place du biparti *)


let rec getIdMetiers metiersList acu = 
  let metiersList = String.trim metiersList in
  Printf.printf "%s\n" metiersList;
  if metiersList = "" then acu
  else Scanf.sscanf metiersList "%d %s@." (fun id rest -> getIdMetiers rest ((-id)::acu))


let read_personne index list line =
  try Scanf.sscanf line "p %s@:%s@." (fun personne metiers-> 
      {
        personnes =
          {
            id = index;
            name = personne;
            metierVoulueId = getIdMetiers metiers [] 
          }::list.personnes;

        metiers = list.metiers
      } )
  with e ->
    Printf.printf "Cannot read metier in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let read_metier index list line = 
  try Scanf.sscanf line "m %d %s" (fun metierId metier ->
      {
        personnes = list.personnes;

        metiers = 
          {
            (* on met en négatif pour pas rentrer en conflits avec les personnes *)
            id = (-metierId);
            name = metier
          }::list.metiers

      } )
  with e ->
    Printf.printf "Cannot read metier in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let createNodePersonne (personne: personne) graph = 
  let graph = new_node graph personne.id in
  let graph = new_arc graph min_int personne.id 1 in 
  graph

let createNodeMetier (metier : metier) graph =
  let graph = new_node graph metier.id in
  let graph = new_arc graph metier.id max_int 1 in 
  graph

let rec createArcBeetweenPersonneAndMetiers personneId metierslist graph =
  match metierslist with 
  | [] -> graph
  | metierId :: reste -> let graph = new_arc graph personneId metierId 1 in 
    let graph = createArcBeetweenPersonneAndMetiers personneId reste graph in 
    graph  


let rec createArcAndNodesForPersonnes personnes graph = 
  match personnes with
  |[] -> graph
  |personne :: reste -> let graph = createNodePersonne personne graph in 
    let graph = createArcBeetweenPersonneAndMetiers personne.id personne.metierVoulueId graph in 
    let graph = createArcAndNodesForPersonnes reste graph in 
    graph     


let rec createArcAndNodesForMetier metiers graph =
  match metiers with
  | [] -> graph 
  | metier :: reste -> let graph = createNodeMetier metier graph in 
    let graph = createArcAndNodesForMetier reste graph in 
    graph

let createGraphFromPersonneAndMetier matchPersonneWithMetier = 
  let graph = empty_graph in 
  let graph = new_node graph max_int in
  let graph = new_node graph min_int in
  let graph = createArcAndNodesForMetier matchPersonneWithMetier.metiers graph in 
  let graph = createArcAndNodesForPersonnes matchPersonneWithMetier.personnes graph in 
  graph

let from_file_bis path =


  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop (n, matchPersonneWithMetier) =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n1, matchPersonneWithMetier1)  =
        (* Ignore empty lines *)
        if line = "" then (n, matchPersonneWithMetier)
        else match line.[0] with
          | 'm' -> (n,read_metier n matchPersonneWithMetier line)
          | 'p' -> (n+1,read_personne n matchPersonneWithMetier line)
          | _-> (n, read_comment matchPersonneWithMetier line)
      in      
      loop (n1, matchPersonneWithMetier1)

    with End_of_file -> matchPersonneWithMetier (* Done *)
  in

  let final_matchPersonneWithMetier = loop (0,initList) in

  close_in infile ;
  final_matchPersonneWithMetier

let generateGraph matchPersonneWithMetier =
    let graph = createGraphFromPersonneAndMetier matchPersonneWithMetier in 
    graph

let rec getPersonnebyID id (personnesList : personne list) =
    match personnesList with
    | [] -> "NotFound"
    | personne :: rest -> if personne.id == id then personne.name else getPersonnebyID id rest

let rec getMetierbyID id (metiersList : metier list) =
    match metiersList with
    | [] -> "NotFound"
    | metiers :: rest -> if metiers.id == id then metiers.name else getMetierbyID id rest
 
let getId id matchPersonneWithMetier =
    let name = getPersonnebyID id matchPersonneWithMetier.personnes in 
    if name = "NotFound" then 
    (
      let name = getMetierbyID id matchPersonneWithMetier.metiers in 
      if name = "NotFound" then (if id = Int.max_int then "Puit" else if id = Int.min_int then "Source" else "NotFound") else name
    )else name

let export_bis path gr matchPersonneWithMetier=
    let infile = open_out path in

    fprintf infile "digraph Test_Graph{\n";
    fprintf infile "rankdir=LR\n";
    fprintf infile "node[shape = circle]\n";
    e_iter gr (fun id1 id2 lbl -> fprintf infile "%s -> %s [label = \"%s\"]\n" (getId id1 matchPersonneWithMetier)(getId id2 matchPersonneWithMetier) lbl);
    fprintf infile "}\n";
    close_out infile ;
    ()

let export path gr =
    let infile = open_out path in

    fprintf infile "digraph Test_Graph{\n";
    fprintf infile "rankdir=LR\n";
    fprintf infile "node[shape = circle]\n";
    e_iter gr (fun id1 id2 lbl -> fprintf infile "%d -> %d [label = \"%s\"]\n" id1 id2 lbl);
    fprintf infile "}\n";
    close_out infile ;
    ()
