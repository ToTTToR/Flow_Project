open Gfile
open Tools
open Printf

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

  (* Open file *)
  let graph = from_file infile in
  let graph1 = gmap graph int_of_string in
  let first_path=match (find_path graph1 [] 0 5) with
    | None -> failwith "Pas de chemin"
    | Some x -> x
  in
  let graph2 = graphe_ecart graph1 first_path in
  let graph3 = gmap graph2 string_of_int in
  let graph4=ford_fulkerson graph 0 5 in
  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph4 in 
  printf "\n";

    ()

