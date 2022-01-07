open Graph
open Tools

type path = id list

val ford_fulkerson: string graph -> id -> id -> string graph

val busacker_gowen: string graph * string graph-> id -> id -> string graph