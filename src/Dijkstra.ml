open Set
open Graph
open Tools
open Printf

type label = (string * int * bool * string) (*Id du noeud,cout du noeud,marké,Id du père*)

module Label = 
  struct
    type t = label
    let compare x y =
      match (x,y) with
        | ((_,cost1,_,_),(_,cost2,_,_)) -> if cost1=cost2 then 0 
        else if cost1<cost2 then -1
        else 1
    end

module Heap = Make(Label)

let s = Heap.empty;;

let print_path list_label = (printf "Label associé : ");List.iter (printf "%d ") list_label;printf("\n")

let dijktrsa gr id1 di2 =
  let init_list gr = n_fold gr (fun liste id -> (id,Int.max_int,false,"")) []
  and t=Heap.add ("id1",2,true,"id2") s in