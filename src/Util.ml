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


