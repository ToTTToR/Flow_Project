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

type path = string

let initList = {personnes=[];metiers=[]} 

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

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
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

let generateGraphPersonneMetier matchPersonneWithMetier = 
  let graph = empty_graph in 
  let graph = new_node graph max_int in
  let graph = new_node graph min_int in
  let graph = createArcAndNodesForMetier matchPersonneWithMetier.metiers graph in 
  let graph = createArcAndNodesForPersonnes matchPersonneWithMetier.personnes graph in 
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

let from_file_personne_metier path =


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

let export_personne_metier path gr matchPersonneWithMetier=
  let infile = open_out path in

  fprintf infile "digraph Test_Graph{\n";
  fprintf infile "rankdir=LR\n";
  fprintf infile "node[shape = circle]\n";
  e_iter gr (fun id1 id2 lbl -> fprintf infile "%s -> %s [label = \"%s\"]\n" (getId id1 matchPersonneWithMetier)(getId id2 matchPersonneWithMetier) lbl);
  fprintf infile "}\n";
  close_out infile ;
  ()