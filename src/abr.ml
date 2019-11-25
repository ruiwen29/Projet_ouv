(** Author Qiwei XIAN *)
(*open Util ;;*)
type abr = Null_abr | Abr of int * abr * abr

type pair = Null_pair | Pair of string * ((int list) ref)

type compressor = Null_com | Compressor of (compressor ref) * (int list) * (compressor ref)

type 'a transition = Transition of ('a * 'a)

type compressor_map = Null_com_map | Compressor_map of ((compressor_map ref) * (string, int)Hashtbl.t * (compressor_map ref))


let print_list l =
    Printf.printf "\"";
    let rec helper arr = 
        match arr with
        | [] -> Printf.printf "\"";
        | x::rest -> 
            Printf.printf "%d " x;
            helper rest;
    in helper l;
;;

let print_arbre arbre =
    Printf.printf "abr : \n";
    let rec dfs arbre = 
        match arbre with
        | Null_abr -> Printf.printf "";
        | Abr(v, left, right) ->
            dfs left;
            Printf.printf "%d " v; 
            dfs right;
    in dfs arbre;
    Printf.printf "\n";
;;

let print_pair (p : pair) = 
    match p with
    | Null_pair -> ();
    | Pair(s, v_list)-> Printf.printf "s: %s : " s; print_list !v_list;
;;

let rec print_pair_list p_List =
    match p_List with
    |[] -> ();
    |a::rest -> print_pair a; 
                print_pair_list rest;
;;


let displayAST (root : abr) =
    print_string "digraph G {\n";
    match root with
    |Null_abr -> ()
    |Abr(v, l, r) ->
        let queue = Queue.create () in
        Queue.push root queue;
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            |Null_abr -> ()
            |Abr(v, l, r) ->
                if (l != Null_abr) then(
                    match l with
                    |Null_abr -> ()
                    |Abr(lv, ll, lr) -> 
                        Printf.printf "%d;\n" lv ;
                        Printf.printf "%d -> %d;\n" v lv;
                        Queue.push l queue;
                );
                match r with
                |Null_abr -> ()
                |Abr(rv, rl, rr) -> 
                    Printf.printf "%d;\n" rv ;
                    Printf.printf "%d -> %d;\n" v rv;
                    Queue.push r queue;
        done;
        print_string "}";
;;

let displayCompressor (root : compressor) = 
    print_string "digraph G {\n";
    let queue = Queue.create () in
    Queue.push root queue;
    let countEdge = Hashtbl.create 10 in    
    match root with
    |Null_com -> ()
    |Compressor(l, v_list, r) ->
        print_list v_list; print_string ";\n";
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            | Null_com -> ()
            | Compressor(l, v_list, r) ->
            if (!l != Null_com) then (
                match !l with 
                | Null_com -> ();
                | Compressor(ll, lv_list, lr) ->
                    let edge = Transition (now, !l) in
                    let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                    if cpt <= 2 then (
                        Hashtbl.replace countEdge edge (cpt + 1) ;
                        print_list lv_list; print_string ";\n";
                        print_list v_list; print_string "->"; print_list lv_list; print_string ";\n";
                        Queue.push !l queue; 
                    );
            );
            match !r with
            | Null_com -> ();
            | Compressor(rl, rv_list, rr) ->
                let edge = Transition(now, !r) in
                let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                if cpt <= 2 then (
                    Hashtbl.replace countEdge edge (cpt + 1) ;
                    print_list rv_list; print_string ";\n";
                    print_list v_list; print_string "->"; print_list rv_list; print_string ";\n";
                    Queue.push !r queue;
                );
    done;
    print_string "}";
;;



let print_nodem (nodem : compressor_map) =
    print_string "\"";
    match nodem with
    |Null_com_map -> Printf.printf "Null\n";
    |Compressor_map(l, map, r) ->
        Hashtbl.iter (fun path value -> Printf.printf " %s:%d " path value) map;
        print_string "\"";
;;

let displayCompressorMap (root : compressor_map) = 
    print_string "digraph G {\n";
    let queue = Queue.create () in
    Queue.push root queue;
    let countEdge = Hashtbl.create 10 in    
    match root with
    |Null_com_map -> ()
    |Compressor_map(l, v_list, r) ->
        print_nodem root; print_string ";\n";
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            | Null_com_map -> ()
            | Compressor_map(l, map, r) ->
            if (!l != Null_com_map) then (
                match !l with 
                | Null_com_map -> ();
                | Compressor_map(ll, l_map, lr) ->
                    let edge = Transition (now, !l) in
                    let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                    if cpt <= 2 then (
                        Hashtbl.replace countEdge edge (cpt + 1) ;
                        print_nodem !l; print_string ";\n";
                        print_nodem now; print_string "->"; print_nodem !l; print_string ";\n";
                        Queue.push !l queue; 
                    );
            );
            match !r with
            | Null_com_map -> ();
            | Compressor_map(rl, rv_list, rr) ->
                let edge = Transition(now, !r) in
                let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                if cpt <= 2 then (
                    Hashtbl.replace countEdge edge (cpt + 1) ;
                    print_nodem !r; print_string ";\n";
                    print_nodem now; print_string "->"; print_nodem !r; print_string ";\n";
                    Queue.push !r queue;
                );
    done;
    print_string "}";
;;



let parse_integers s =
    let stream = (Scanf.Scanning.from_file s) in
    let rec do_parse acc =
      try
        do_parse (Scanf.bscanf stream " %c " (fun x -> ());  
                 Scanf.bscanf stream " %d " (fun x -> x :: acc)) (*只能识别int*)
      with      
      | End_of_file -> acc
    in List.rev (do_parse [])
;;




(* Question 1.1 *)
let rec remove_at list n =
    match list with
    | [] -> []
    | head::rest -> 
        if n == 0 then rest
        else head::remove_at rest (n - 1)
;;

let rec makelist n =
    if n == 0 then []
    else n::makelist (n - 1)
;;

let extraction_alea l p =
    match l with
    | [] -> l, p
    | head::rest ->
        let len = List.length l in
        let r = Random.int len in
        (remove_at l r), ((List.nth l r)::p)
;;

(* Question 1.2 *)
let gen_permutation n =
    let l = makelist n in
    let rec helper l p n =
        if n == 0 then p
        else 
            let l, p = extraction_alea l p in
            helper l p (n - 1)
    in helper l [] n;
;;

(* Question 1.3 *)
let rec ajoute (arbre : abr) value =
    match arbre with
    | Null_abr -> Abr(value, Null_abr, Null_abr)
    | Abr(v, left, right) ->
    if value < v then Abr(v, ajoute left value, right)
    else Abr(v, left, ajoute right value)
;;
let construct_ast list = 
    let rec helper arbre list = 
        match list with
        | [] -> arbre
        | x::rest -> helper (ajoute arbre x) rest
    in helper Null_abr list;
;;

let search arbre key =
    let rec dfs arbre key =
    match arbre with
    | Null_abr -> false
    | Abr(v, left, right) ->
        if v == key then true
        else if key < v then dfs left key
        else dfs right key
    in dfs arbre key;
;;


(* Question 2.4 *)
let rec abr_mot (a: abr) =
    match a with
    | Null_abr -> ""
    | Abr(v, l, r)-> "(" ^ (abr_mot l) ^ ")" ^ (abr_mot r)
;;


let modify_list list v =
    match !list with
    |[] -> list := [v]
    |a::rest -> list := v::a::rest
;;

let rec ajoute_mot_value mot valeur list head =
    match !list with
    |[] -> modify_list head (Pair(mot, ref [valeur])); 
    |Null_pair::rest -> ajoute_mot_value mot valeur (ref rest) head
    |Pair(s, v_list)::rest ->
        if (compare s mot == 0) then modify_list v_list valeur
        else ajoute_mot_value mot valeur (ref rest) head
;;

let tree_traversal (abr : abr) =
    let list = ref [] in
    let lib = ref (Hashtbl.create 16) in
    let rec helper abr list lib =
    match abr with
    |Null_abr -> "";
    |Abr(v, l, r)->
        let mot = "(" ^ (helper l list lib) ^ ")" ^ (helper r list lib) in
        Hashtbl.add !lib v mot ;
        ajoute_mot_value mot v list list;
        mot
    in  (fun x -> ())(helper abr list lib); 
    list, lib
;;


let make_compressor (pair : pair)  =
    match pair with
    |Null_pair -> Null_com
    |Pair(s, v_list) -> Compressor(ref Null_com, !v_list, ref Null_com) 
;;

let rec print_list_list (l : (int list) list) =
    match l with
    | [] -> ()
    | subl::rest -> print_list subl; print_list_list rest;
;;

let rec print_compressor (node : compressor) = 
    match node with
    | Null_com -> Printf.printf "*\n";
    | Compressor(left, v_list, right) -> 
        print_compressor !left; 
        print_list v_list; 
        print_compressor !right;
;;

let pairList_to_map (list : pair list) =
    let lib = ref (Hashtbl.create 16) in
    let rec helper (list : pair list) lib = 
        match list with
        |[] -> ()
        |Null_pair::rest -> helper rest lib 
        |Pair(s, v_list)::rest-> 
            let tmp = ref (make_compressor (Pair(s, v_list))) in
            Hashtbl.add !lib s tmp;
            helper rest lib;
    in helper list lib; lib
;;

let connect_node (abr : abr) 
(lib : (string, compressor ref) Hashtbl.t ref ) 
(value_mot : (int, string) Hashtbl.t ref) =
    let bfs (abr : abr) 
    (lib : (string, compressor ref) Hashtbl.t ref ) 
    (value_mot : (int, string) Hashtbl.t ref) = 
        match abr with
        | Null_abr -> ()
        | Abr(v, l, r) -> 
            let queue = Queue.create () in
            Queue.push (Abr(v, l, r)) queue;
            while not (Queue.is_empty queue) do
                let node = Queue.pop queue in
                match node with
                | Null_abr -> ();
                | Abr(v, l, r) ->
                let mot_v = Hashtbl.find !value_mot v in
                let father_noeud = Hashtbl.find !lib mot_v in
                if (l != Null_abr) then(
                    match l with 
                    | Null_abr -> ();
                    | Abr(vl, _, _) -> 
                        let mot_l = Hashtbl.find !value_mot vl in
                        let nodel = Hashtbl.find !lib mot_l in
                        match !father_noeud with
                        |Null_com -> ();
                        |Compressor(fg, _, fd) -> fg:=!nodel;
                        Queue.push l queue;
                ); 
                match r with
                | Null_abr -> ();
                | Abr(vr, _, _) ->
                    let mot_r = Hashtbl.find !value_mot vr in
                    let noder = Hashtbl.find !lib mot_r in
                    match !father_noeud with
                    |Null_com -> ();
                    |Compressor(fg, _, fd) -> fd:=!noder;
                    Queue.push r queue;
            done
        in bfs abr lib value_mot;
;;

(* Question 2.5 *)
let compress_ast (abr : abr) =
    match abr with
        | Null_abr -> Null_com
        | Abr(v,_,_) -> 
            let pair_list, value_mot = tree_traversal abr  in
            let mot_noeud = pairList_to_map !pair_list  in
            connect_node abr mot_noeud value_mot;
            let mot_root = Hashtbl.find !value_mot v in
            !(Hashtbl.find !mot_noeud mot_root)
;;


let rec search (list: int list) (value : int) = 
    match list with
    | [] -> false
    | x::rest -> 
        if x = value then true else search rest value 
;;

(* Question 2.6 *)
let rec search_compressor (com : compressor) (value : int) =
    match com with
    | Null_com -> false
    | Compressor(l, v_list,r) ->
        match v_list with
        |[] -> false
        |x::rest -> 
            if value < x then search_compressor !l value
            else
            let res = search rest value in
            if not res then search_compressor !r value
            else res
;;


(*Question 2.7*)

(*把一个树遍历然后返回他的括号*hashtab(标签*数值)*)
let tree_traversal_map (abr : abr) =
  let list = ref [] in
  let lib = ref (Hashtbl.create 16) in
  let map = ref (Hashtbl.create 16) in
  let rec helper abr list lib map path=
  match abr with
  |Null_abr -> "";
  |Abr(v, l, r)->
      let mot = "(" ^ (helper l list lib map (path^"a")) ^ ")" ^ (helper r list lib map (path ^"b")) in
      Hashtbl.add !map v path;
      Hashtbl.add !lib v mot ;
      ajoute_mot_value mot v list list;
      mot
  in  (fun x -> ())(helper abr list lib map "a"); 
  list, lib , map
;;

let make_compressor_map (pair : pair) (int_path: (int, string)Hashtbl.t) =
  let path_int = Hashtbl.create 16 in 
  match pair with
  |Null_pair -> ref Null_com_map
  |Pair(s, v_list) -> 
    List.iter (fun x -> Hashtbl.add path_int (Hashtbl.find int_path x) x) (!v_list);
    ref (Compressor_map(ref Null_com_map, path_int, ref Null_com_map))
;;


let pairList_to_Node_map (pairList : pair list) (int_path: (int, string)Hashtbl.t) = 
    let mot_node = ref (Hashtbl.create 16) in
    let rec helper (pairList : pair list) (int_path: (int, string)Hashtbl.t) (mot_node : (string, compressor_map ref)Hashtbl.t ref) = 
        match pairList with
        |[] -> ()
        |pair::rest -> 
            match pair with
            |Null_pair -> (helper rest int_path mot_node;)
            |Pair(s, _) -> Hashtbl.add !mot_node s (make_compressor_map pair int_path);
            helper rest int_path mot_node;
    in helper pairList int_path mot_node; 
    mot_node
;;

let connect_node_map (abr: abr)
(mot_nodemap : (string, compressor_map ref)Hashtbl.t ref) 
(value_mot : (int, string) Hashtbl.t ref) =
    let bfs (abr : abr)
    (mot_nodemap : (string, compressor_map ref) Hashtbl.t ref ) 
    (value_mot : (int, string) Hashtbl.t ref) = 
        match abr with
        | Null_abr -> ()
        | Abr(v, l, r) -> 
            let queue = Queue.create () in
            Queue.push (Abr(v, l, r)) queue;
            while not (Queue.is_empty queue) do
                let node = Queue.pop queue in
                match node with
                | Null_abr -> ();
                | Abr(v, l, r) ->
                let mot_v = Hashtbl.find !value_mot v in
                let father_noeud = Hashtbl.find !mot_nodemap mot_v in
                if (l != Null_abr) then(
                    match l with 
                    | Null_abr -> ();
                    | Abr(vl, _, _) -> 
                        let mot_l = Hashtbl.find !value_mot vl in
                        let nodel = Hashtbl.find !mot_nodemap mot_l in
                        match !father_noeud with
                        |Null_com_map -> ();
                        |Compressor_map(fg, _, fd) -> fg:=!nodel;
                        Queue.push l queue;
                ); 
                match r with
                | Null_abr -> ();
                | Abr(vr, _, _) ->
                    let mot_r = Hashtbl.find !value_mot vr in
                    let noder = Hashtbl.find !mot_nodemap mot_r in
                    match !father_noeud with
                    |Null_com_map -> ();
                    |Compressor_map(fg, _, fd) -> fd:=!noder;
                    Queue.push r queue;
            done
        in bfs abr mot_nodemap value_mot;
;;
        

let compress_ast2 (abr : abr) =
    match abr with
        | Null_abr -> Null_com_map
        | Abr(v,_,_) -> 
            let pair_list, int_mot, int_path = tree_traversal_map abr  in
            let mot_noeud = pairList_to_Node_map !pair_list !int_path  in
            connect_node_map abr mot_noeud int_mot;
            let mot_root = Hashtbl.find !int_mot v in
            !(Hashtbl.find !mot_noeud mot_root)
;;


let search (root : compressor_map) (value : int) = 
    let rec helper root path value =
        match root with
        | Null_com_map -> false
        | Compressor_map(left, map, right) -> 
            let target = Hashtbl.find map path in
            if target = value then true
            else if value < target then helper !left (path^"a") value
            else helper !right (path^"b") value
    in  helper root "a" value

;;
        

let compressor_list_ast (test_list : int list) = compress_ast (construct_ast test_list) ;;

let compressor_map_ast (test_list : int list) = compress_ast2 (construct_ast test_list) ;; 

(* ------- main ------- *)
let () = 
    (* let test_list = gen_permutation 50 in *)
    (* let test_list = [4; 2; 1; 3; 8; 6; 5; 7; 9] in *)
    (* let a = construct_ast Empty a_list in      *)
    Printf.printf "start : %f\n" (Gc.allocated_bytes ());
    let file = "../Jeu_de_tests/donnee10000.txt" in
    let test_list = parse_integers file in
    (* let gc = Gc.stat () in *)
    (* let root = compressor_list_ast test_list in *)
    (* let root = compressor_map_ast test_list in  *)
    let ast = construct_ast test_list in
    Printf.printf "end: %f\n" (Gc.allocated_bytes ());
    ;;
let () = Statmemprof_emacs.start 1E-4 30 5;
;;
            