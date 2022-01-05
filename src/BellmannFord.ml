open Queue
open Graph 
open Tools
open Printf

type label = (int*int*int) (*Id du noeud,cout du noeud, Id du père*)
let file = Queue.create ();;

let replace liste id elem = List.map (fun x -> let (id1,_,_)=x in if id1=id then elem else x) liste
let find_label liste_label id1 = List.find (fun x->let (id,_,_)=x in id=id1) liste_label
let print_list_label list_label = printf "Label associé : ";List.iter (fun (id,cost,father) -> (printf "(%d," id);(printf "%d," cost);(printf "%d) " father)) list_label;printf("\n")
let exists x q = Queue.fold (fun acc el -> (x=el)||acc) false q
let create_path gr label_list id1 id2 =
  match find_label label_list id2 with
  | (_,_,-1) -> None
  | (_,_,id_father) -> 
    let rec helper id_list= function
     | (id,_,father_h) -> if id=id1 then id::id_list else helper (id::id_list) (find_label label_list father_h)
    in
  Some  (helper [] (find_label label_list id2))

let print_path = function
  | None -> printf "No path!\n%!"
  | Some list_id -> List.iter (fun x -> printf "%d\n%!" x) list_id

let bellmanford gr id1 id2 =
  let init_list = n_fold gr (fun liste id -> (id,Int.max_int,-1)::liste) [] in
  let first_list = replace init_list id1 (id1,0,-1) in
  Queue.add id1 file;
  let rec loop_file l_label k =
    if (k>10) then failwith "Stop algo" else
    match Queue.take_opt file with
    | None -> printf "File vide!\n\n\n%!";l_label
    | Some elt -> printf "Element extrait de la file : %d \n%!" elt;print_list_label l_label;
    
    let rec loop_suc label_l label_pred = function
    | [] -> label_l
    | (id_succ1,len)::rest -> printf "Id successeur : %d et cout arc : %d\n%!" id_succ1 len;
      let label_succ = (find_label label_l id_succ1) in
      let (id_succ,cost_succ,father_succ)=label_succ 
      and (id_pred,cost_pred,father_pred)=label_pred in

      if (cost_pred+len)<cost_succ then 
        let new_label_succ=(id_succ,(cost_pred+len),id_pred) in
        let next_list=replace label_l id_succ new_label_succ in 
        if exists id_succ file then loop_suc next_list label_pred rest
        else (printf "Élément ajouté dans la file : %d \n%!" id_succ; Queue.add id_succ file; loop_suc next_list label_pred rest)
      else loop_suc label_l label_pred rest
    in
    let (label_list_bis) = loop_suc l_label (find_label l_label elt) (out_arcs gr elt) in
    loop_file label_list_bis (k+1)
  in 
  create_path gr (loop_file first_list 0) id1 id2
