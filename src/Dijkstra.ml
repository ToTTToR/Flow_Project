open Set
open Graph
open Tools

type label = (string * int * )

module Label = 
  struct
    type t = (int * int * )
    let compare x y =
      match (x,y) with
        | ((_,cost1),(_,cost2)) -> if cost1=cost2 then 0 
        else if cost1<cost2 then -1
        else 1
    end

module Heap = Make(Label)

let s = Heap.empty;;

let disjktra gr id1 di2 =
