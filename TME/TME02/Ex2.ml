type seance = (string * int * int)

type prog = seance list


let rec ajout_film (name : string ) (l_time : (int * int ) list ) (l_vide :  prog ) : prog =
    List.fold_right 

        (fun  (a,b) acc  -> ( name ,a,b ) :: acc )  
    
        (List.rev l_time)
        l_vide
let seance_film (n : string ) (s : seance) : bool =
    let (a,b,c) = s in
    if a = n then true 
    else false

(*
let seance_film (n : string) ((a,_,_) : seance) : bool =
    a = n    
    :)
*)

let liste_seances (n : string ) (p : prog) : prog =
    List.filter 
        ( fun x -> seance_film n x ) p 
(*
let liste_seances (n : string) (p : prog) : prog =
    List.filter (seance_film n) p

let liste_seances (n : string) (p : prog) : prog =
    List.filter
        (fun (name, start_time, end_time) -> name = n)
        p
  
*)

let seances_compat (s1 : seance ) ( s2 : seance) : bool =
    let (_,s1,e1),(_,s2,e2) = s1 , s2 in 
    if e1 < s2 then true 
    else if e2 < s1 then true 
    else false 


(*

let seances_compat2 ( (_,s1,e1) : seance  ) ( (_,s2,e2) : seance ) : bool = 
    if e1 < s2 then true 
    else if e2 < s1 then true 
    else false 

let seances_compat ((_,s1,e1) : seance) ((_,s2,e2) : seance) : bool =
    e1 < s2 || e2 < s1 

    *)


let liste_compat1 (s : seance )  (p : prog) : prog =
    List.filter (  seances_compat s  ) p 

(*
List.filter (         seances_compat s  ) p = 
List.filter (fun x -> seances_compat s x) p
*)

let map_paire (a : 'a) ( l : 'b list ) : ('a * 'b) list = 
    List.map ( fun x -> (a,x) ) l 


