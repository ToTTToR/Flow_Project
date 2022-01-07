(* Algo qui ne sert à rien pour ce projet... *)

open Set
open Graph
open Tools
open Printf

type label = (int * int * bool * int) (*Id du noeud,cout du noeud,marké,Id du père*)

module Label = 
  struct
    type t = label
    let compare x y =
      match (x,y) with
        | ((id1,cost1,_,_),(id2,cost2,_,_)) ->
        if id1=id2 then 0 else if (cost1-cost2)<=0 then -1 else 1
    end

module Heap = Make(Label)

let print_list_label list_label = printf "Label associé : ";List.iter (fun (id,cost,marked,father) -> (printf "(%d," id);(printf "%d," cost);(printf "%b," marked);(printf "%d) " father)) list_label;printf("\n")

(*Auxiliary functions to solve shortest path algorithm with Dijkstra*)
let replace liste id elem = List.map (fun x -> let (id1,_,_,_)=x in if id1=id then elem else x) liste
let find_label liste_label id1 = List.find (fun x->let (id,_,_,_)=x in id=id1) liste_label

(*Create a list of ids after that dijkstra algorithm solved the problem*)
let create_path gr label_list id1 id2 =
  match find_label label_list id2 with
  | (_,_,false,_) -> None
  | (_,_,true,_) -> 
    let rec helper id_list= function
     | (id,_,_,father_h) -> if id=id1 then id::id_list else helper (id::id_list) (find_label label_list father_h)
    in
  Some  (helper [] (find_label label_list id2))

let print_heap s = (Heap.iter (fun (id,cost,marked,father) -> (printf "(%d," id);(printf "%d," cost);(printf "%b," marked);(printf "%d) " father)) s);printf("\n")

let dijkstra gr id1 id2 =
  let gr1=gmap gr int_of_string in
  let init_list = n_fold gr1 (fun liste id -> (id,Int.max_int,false,id)::liste) [] in(*Liste des labels*)
  let first_list = replace init_list id1 (id1,0,false,id1) in (*Changer le label du noeud d'origine*)
  let s=Heap.add (find_label first_list id1) Heap.empty in (*Mettre le sommet d'origine dans tas*)
  
  let rec loop_heap tas l_label =
    match Heap.min_elt_opt tas with
    | None -> printf "Tas vide\n";l_label
    | Some elt -> let (id,cost,_,father)=elt in
    if id=id2 then replace l_label id (id,cost,true,father)else
    let next_tas=Heap.remove elt tas 
    and next_label_list=replace l_label id (id,cost,true,father) in
    
    let rec loop_succ heap label_l label_pred= function
    | [] -> (heap,label_l)
    | (id1,len)::rest -> if len=0 then loop_succ heap label_l label_pred rest else
      let label_succ=(find_label label_l id1) in 
      let (id_succ,cost_succ,marked_succ,father_succ)=label_succ 
      and (id_pred,cost_pred,marked_pred,father_pred)=label_pred in
      
      if marked_succ then loop_succ heap label_l label_pred rest else 
      if cost_succ > (cost_pred+len) then 
        let new_label_succ=(id_succ,(cost_pred+len),marked_succ,id_pred) in printf "Successeur : %d\n" id_succ; 
        let next_heap=Heap.add new_label_succ (Heap.remove label_succ heap)
        and next_list=replace label_l id_succ new_label_succ in
        loop_succ next_heap next_list label_pred rest
      else loop_succ heap label_l label_pred rest
    in
    print_heap tas;
    let (tas_bis,label_list_bis)=loop_succ next_tas next_label_list elt (out_arcs gr1 id) in
    loop_heap tas_bis label_list_bis
  in
  create_path gr1 (loop_heap s first_list) id1 id2