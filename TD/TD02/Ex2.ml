let rec verifie ( lst  : ( int *  int )   list) : bool =
    match lst with
    | [] -> true 
    | [_] -> true  (*list with exactly one element*)
    | (a,b) :: (c,d) :: t -> (c+d > a+b) && verifie ((c,d) :: t) (* a,b and c,d are the first and two element of the list t is the tail the rest of the list *)

let rec alloue (n : int ) (lst : ( int * int ) list) :  (int * int) list =
    match lst with
    | [] -> []
    | (a,b) :: t -> 
        if b = n then t
        else 
            if b > n then 
                (a+n , b -n) :: t 
            else (a,b) :: alloue n t

let rec libere (addr : int ) (size : int) (lst : (int * int )  list) : (int*int)  list =
    match lst with
    | [] -> []
    | (a,b) :: t -> 
        if addr + size = a then
            (addr, size  +b ) :: t
        else 
            if addr > a then 
                (a,b) :: libere addr size t
            else
                (addr,size) :: (a,b) ::  t 
(*
empty = insert 
equal = merge 
bigger = move forward 
smaller (else) = insert here 

*)

let rec defragmente (lst : (int * int) list) : (int * int ) list =
    match lst with
    | [] -> []
    | [_] -> lst
    | (a,b) :: (c,d) ::  t -> 
        if a+b = c then 
            defragmente ((a , b + d) :: t) 
        else 
            (a,b) :: defragmente ((c,d) :: t)

let defragmente_right (lst : (int * int) list) : (int * int) list =
    List.fold_right
        (fun (a, b) acc ->
            match acc with
            | [] -> [(a, b)]
            | (c, d) :: t ->
                if a + b = c then
                    (a, b + d) :: t
                else
                    (a, b) :: acc)
        lst
        []


let films =
[("Le comte de Monte Cristo", [("Delaporte","Matthieu");("De LaPatelliere","Alexandre")],2024,[("Niney","Pierre");("Bouillon","Bastien");("Demoustier","Anais")])
;
("Le Temps D’Aimer", [("Quillevere","Katell")],2023,[("Demoustier","Anais");("Lacoste","Vincent");("Beaurepaire","Paul")]);

("Yves Saint-Laurent", [("Lespert","Jalil")],2014,[("Niney","Pierre");("Gallienne","Guillaume");("LeBon","Charlotte")]);

("Falcon Lake", [("Le Bon","Charlotte")],2022,[("Engel","Joseph");("Montpetit","Sara");("Chokri","Monia")]);

("Un p’tit truc en plus", [("Artus","")],2024,[("Artus", "");("Cornillac","CLovis");("Belaide","Alice")]);
]

type t_film = (string * (string * string) list * int * (string * string) list) list

let list_titres (film : t_film) : string list = 
    List.map (fun (x , _ , _ , _) -> x) film

let filme_annee (film : t_film) (n : int): string list =
    list_titres (   List.filter (  fun (_ , _ , x ,  _ ) -> x =  n) film   )


let filme_annee2 (film : t_film ) (n : int) : string list =
    List.fold_left   (

        fun acc (titre , _ , annee , _ ) -> 
            if annee = n then
                acc @ [titre]
            else 
                acc

    ) [] film

