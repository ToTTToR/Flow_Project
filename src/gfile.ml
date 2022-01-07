open Graph
open Printf

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

(*Used for flow max/min cost algorithm *)
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

let export path gr =
  let infile = open_out path in

  fprintf infile "digraph Test_Graph{\n";
  fprintf infile "graph [pad=\"0.5\", nodesep=\"0.5\", ranksep=\"2\"];\n";
  fprintf infile "rankdir=LR\n";
  fprintf infile "node[shape = circle]\n";
  e_iter gr (fun id1 id2 lbl -> fprintf infile "%d -> %d [label = \"%s\"]\n" id1 id2 lbl);
  fprintf infile "}\n";
  close_out infile ;
  ()
