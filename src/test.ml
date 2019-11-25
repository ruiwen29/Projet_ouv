(* exemple *)

module MyUsers = Map.Make(String);;
let m = MyUsers.empty;;
let m = MyUsers.add "fred" "sugarplums" m;;
let m = MyUsers.add "tom" "ilovelucy" m;;
let m = MyUsers.add "mark" "ocamlrules" m;;
let m = MyUsers.add "pete" "linux" m;;

let print_user key password =
  print_string(key ^ " " ^ password ^ "\n");;

  MyUsers.iter print_user m;;

MyUsers.find "fred" m;; (*找键值*)
type map = None | Nodem of string * ((string, int) Hashtbl.t ref)



(** Author Qiwei XIAN *)
type abr = Empty | Node of int * abr * abr

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
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
    if value < v then Node(v, ajoute left value, right)
    else Node(v, left, ajoute right value)
;;

let rec ajouteList arbre list = 
    match list with
    | [] -> arbre
    | x::rest -> ajouteList (ajoute arbre x) rest
;;

let search arbre key =
    let rec dfs arbre key =
    match arbre with
    | Empty -> false
    | Node(v, left, right) ->
        if v == key then true
        else if key < v then dfs left key
        else dfs right key
    in dfs arbre key;
;;

let printArbre arbre =
    Printf.printf "abr : \n";
    let rec dfs arbre = 
        match arbre with
        | Empty -> Printf.printf "";
        | Node(v, left, right) ->
            dfs left;
            Printf.printf "%d " v; 
            dfs right;
    in dfs arbre;
    Printf.printf "\n";
;;
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

(* Question 2.4 *)
let rec abr_mot (a: abr) =
    match a with
    | Empty -> ""
    | Node(v, l, r)-> "(" ^ (abr_mot l) ^ ")" ^ (abr_mot r)
;;

(* let isomorphe (a: abr) (b: abr) = 
    let am = abr_mot a in
    let bm = abr_mot b in
    Printf.printf "a: %s\n" am;
    Printf.printf "b: %s\n" bm;
    am == bm
;; *)

type pair = None | Nodep of string * ((int list) ref)
let print_pair p = 
    match p with
    | None -> ();
    | Nodep(s, v_list)-> Printf.printf "s: %s : " s; print_list !v_list;
;;

let rec print_pair_list p_List =
    match p_List with
    |[] -> ();
    |a::rest -> print_pair a; 
                print_pair_list rest;
;;

let modify_list list v =
    match !list with
    |[] -> list := [v]
    |a::rest -> list := v::a::rest
;;

let rec ajoute_mot_value mot valeur list head =
    match !list with
    |[] -> modify_list head (Nodep(mot, ref [valeur])); 
    |None::rest -> ajoute_mot_value mot valeur (ref rest) head
    |Nodep(s, v_list)::rest ->
        if (compare s mot == 0) then modify_list v_list valeur
        else ajoute_mot_value mot valeur (ref rest) head
;;

let tree_traversal (abr : abr) =
    let list = ref [] in
    let lib = ref (Hashtbl.create 16) in
    let rec helper abr list lib =
    match abr with
    |Empty -> "";
    |Node(v, l, r)->
        let mot = "(" ^ (helper l list lib) ^ ")" ^ (helper r list lib) in
        Hashtbl.add !lib v mot ;
        ajoute_mot_value mot v list list;
        mot
    in  (fun x -> ())(helper abr list lib); 
    list, lib
;;

type compressor = None | Noeud of (compressor ref) * (int list) * (compressor ref)

(* let rec list_to_llist l = 
    match l with
    |[] -> []
    |a::rest -> [a]::list_to_llist rest
;; *)

let make_compressor (pair : pair)  =
    match pair with
    |None -> None
    |Nodep(s, v_list) -> Noeud(ref None, !v_list, ref None) 
;;

let rec print_list_list (l : (int list) list) =
    match l with
    | [] -> ()
    | subl::rest -> print_list subl; print_list_list rest;
;;

let rec print_compressor (pair : compressor) = 
    match pair with
    | None -> Printf.printf "*\n";
    | Noeud(left, v_list, right) -> 
        print_compressor !left; 
        print_list v_list; 
        print_compressor !right;
;;

let pairList_to_map (list : pair list) =
    let lib = ref (Hashtbl.create 16) in
    let rec helper (list : pair list) lib = 
        match list with
        |[] -> ()
        |None::rest -> helper rest lib 
        |Nodep(s, v_list)::rest-> 
            let tmp = ref (make_compressor (Nodep(s, v_list))) in
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
        | Empty -> ()
        | Node(v, l, r) -> 
            let queue = Queue.create () in
            Queue.push (Node(v, l, r)) queue;
            while not (Queue.is_empty queue) do
                let node = Queue.pop queue in
                match node with
                | Empty -> ();
                | Node(v, l, r) ->
                let mot_v = Hashtbl.find !value_mot v in
                let father_noeud = Hashtbl.find !lib mot_v in
                if (l != Empty) then(
                    match l with 
                    | Empty -> ();
                    | Node(vl, _, _) -> 
                        let mot_l = Hashtbl.find !value_mot vl in
                        let nodel = Hashtbl.find !lib mot_l in
                        match !father_noeud with
                        |None -> ();
                        |Noeud(fg, v_List_list, fd) -> fg:=!nodel;
                        Queue.push l queue;
                ); 
                match r with
                | Empty -> ();
                | Node(vr, _, _) ->
                    let mot_r = Hashtbl.find !value_mot vr in
                    let noder = Hashtbl.find !lib mot_r in
                    match !father_noeud with
                    |None -> ();
                    |Noeud(fg, v_List_list, fd) -> fd:=!noder;
                    Queue.push r queue;
            done
        in bfs abr lib value_mot;
;;

(* Question 2.5 *)
let compress_ast (abr : abr) =
    match abr with
        | Empty -> None
        | Node(v,_,_) -> 
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
    | None -> false
    | Noeud(l, v_list,r) ->
        match v_list with
        |[] -> false
        |x::rest -> 
            if value < x then search_compressor !l value
            else
            let res = search rest value in
            if not res then search_compressor !r value
            else res
;;

let displayAST (root : abr) =
    print_string "digraph G {\n";
    match root with
    |Empty -> ()
    |Node(v, l, r) ->
        let queue = Queue.create () in
        Queue.push root queue;
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            |Empty -> ()
            |Node(v, l, r) ->
                if (l != Empty) then(
                    match l with
                    |Empty -> ()
                    |Node(lv, ll, lr) -> 
                        Printf.printf "%d;\n" lv ;
                        Printf.printf "%d -> %d;\n" v lv;
                        Queue.push l queue;
                );
                match r with
                |Empty -> ()
                |Node(rv, rl, rr) -> 
                    Printf.printf "%d;\n" rv ;
                    Printf.printf "%d -> %d;\n" v rv;
                    Queue.push r queue;
        done;
        print_string "}";
;;

type transition = Edge of (compressor * compressor)
let displayCompressor (root : compressor) = 
    print_string "digraph G {\n";
    let queue = Queue.create () in
    Queue.push root queue;
    let countEdge = Hashtbl.create 10 in    
    match root with
    |None -> ()
    |Noeud(l, v_list, r) ->
        print_list v_list; print_string ";\n";
        while not (Queue.is_empty queue) do
            let now = Queue.pop queue in
            match now with
            | None -> ()
            | Noeud(l, v_list, r) ->
            if (!l != None) then (
                match !l with 
                | None -> ();
                | Noeud(ll, lv_list, lr) ->
                    let edge = Edge (now, !l) in
                    let cpt  = if (Hashtbl.mem countEdge edge == false) then 1 else Hashtbl.find countEdge edge in
                    if cpt <= 2 then (
                        Hashtbl.replace countEdge edge (cpt + 1) ;
                        print_list lv_list; print_string ";\n";
                        print_list v_list; print_string "->"; print_list lv_list; print_string ";\n";
                        Queue.push !l queue; 
                    );
            );
            match !r with
            | None -> ();
            | Noeud(rl, rv_list, rr) ->
                let edge = Edge(now, !r) in
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

type compressor_map = Null | Noeudm of ((compressor_map ref) * (string, int)Hashtbl.t * (compressor_map ref))


let parse_integers s =
    let stream = (Scanf.Scanning.from_string s) in
    let rec do_parse acc =
      try
        do_parse (Scanf.bscanf stream " %c " (fun x -> ());  
                 Scanf.bscanf stream " %d " (fun x -> x :: acc)) (*只能识别int*)
      with      
      | End_of_file -> acc
    in List.rev (do_parse [])
;;



(*Question 2.7*)

(*把一个树遍历然后返回他的括号*hashtab(标签*数值)*)
let tree_traversal_map (abr : abr) =
  let list = ref [] in
  let lib = ref (Hashtbl.create 16) in
  let map = ref (Hashtbl.create 16) in
  let rec helper abr list lib map path=
  match abr with
  |Empty -> "";
  |Node(v, l, r)->
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
  |None -> Null
  |Nodep(s, v_list) -> 
    List.iter (fun x -> Hashtbl.add path_int (Hashtbl.find int_path x) x) (!v_list);
    ( Noeudm(ref Null, path_int, ref Null))
;;

let pairList_to_Node_map (pairList : pair list) (int_path :(int, string)Hashtbl.t) =
  let lib = ref (Hashtbl.create 16) in 
  let rec helper(pairlist :pair list)  lib  = 
      match pairlist with
      |[] -> ()
      |None::rest -> helper rest lib 
      |Nodep(s, v_list)::rest-> 
        let tmp = ref(make_compressor_map(Nodep(s,v_list)) int_path ) in
        Hashtbl.add !lib s tmp;
        helper rest lib;
  in helper pairList lib;
  lib
  ;;

  let print_nodem (nodem : compressor_map) =
    match nodem with
    |Null -> Printf.printf "Null\n";
    |Noeudm(l, map, r) ->
        Hashtbl.iter (fun path value -> Printf.printf "k: %s, v: %d" path value) map;
;;
      





(* ------- main ------- *)
let () = 
    (* let a_list = gen_permutation 30 in *)
    (* let a_list = [4; 2; 1; 3; 8; 6; 5; 7; 9] in *)
    (* let a = ajouteList Empty a_list in      *)
    
 (*   let file = "/home/xian/Projets/M1-S1/Project_ouv/Jeu_de_tests/donnee150.txt" in
    let test_list = parse_integers file in
    let a = ajouteList Empty test_list in
    let root = compress_ast a in 
    displayCompressor root
;;*)   
(*
    let tab = "[1, 2, 3, 4, 5, 6, 7, 8, 9]"  in
    let test_list = parse_integers tab in
    let a = ajouteList Empty test_list in
    let root = compress_ast a in 
    displayCompressor root
    *)
    let test_list = [4; 2; 1; 3; 8; 6; 5; 7; 9] in
    let a = ajouteList Empty test_list in
    let pairList, int_mot, int_path = tree_traversal_map a in
    let mot_node = pairList_to_Node_map !pairList !int_path in
     Hashtbl.iter (fun key node -> print_string key; print_nodem !node) !mot_node; 