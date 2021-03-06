open Graph
open Printf
open Tools

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

val from_file_personne_metier: path -> matchPersonneWithMetier
val generateGraphPersonneMetier: matchPersonneWithMetier -> string graph
val export_personne_metier: path -> string graph -> matchPersonneWithMetier -> unit

