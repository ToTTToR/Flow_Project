open Gfile
open Printf
open Flow_Algo
open Tools
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

  let labels = [("id1",0,false,"");("id2",0,false,"");("id3",0,false,"")] in 
    

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = Int.min_int
  and _sink = Int.max_int
  in

  (* Open file *)
  let fromFile = from_file_bis infile in
  let graph = generateGraph fromFile in
  let graph = gmap graph string_of_int in
  let graph4= ford_fulkerson graph _source _sink in
  (* Rewrite the graph that has been read. *)
  let () = export_bis outfile graph4 fromFile in 
  printf "\n";

  ()

