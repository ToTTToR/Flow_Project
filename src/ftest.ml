open Gfile
open Printf
open Flow_Algo
open Dijkstra
open BellmannFord

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
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in
  export "init" (from_file infile); 
  (* Open file *)
  if infile = "graphs/graph3" || infile="graphs/graph4" then
    let graph = busacker_gowen (from_file_bis infile) _source _sink in
    export outfile graph;
    (*let (graph_flow,_) = (from_file_bis infile) in
    let graph2 = ford_fulkerson graph_flow _source _sink in
    export outfile graph2*)
  else 
    let graph = ford_fulkerson (from_file infile) _source _sink in
    export outfile graph
  (*let () = export outfile graph in 

  ()*)

