open Gfile
open Printf
open Flow_Algo
open Tools
open PersonneMetier
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
    

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = Int.min_int
  and _sink = Int.max_int
  in

  (* Partie projet visualisation, d'un graphe de personne/métier *)
  
  let fromFile = from_file_personne_metier infile in
  let graph = generateGraphPersonneMetier fromFile in
  let graph = ford_fulkerson graph _source _sink in
  export_personne_metier outfile graph fromFile 

  (*Partie projet avec ajout de contraintes (cout) *)

  export "init" (from_file infile); 
  if infile = "graphs/graph3" || infile="graphs/graph4" then
    let graph = busacker_gowen (from_file_bis infile) _source _sink in
    export outfile graph;
    (*Lignes de code pour tester sans la contrainte *)
    (*let (graph_flow,_) = (from_file_bis infile) in
    let graph2 = ford_fulkerson graph_flow _source _sink in
    export outfile graph2*)
  else 
    let graph = ford_fulkerson (from_file infile) _source _sink in
    export outfile graph
