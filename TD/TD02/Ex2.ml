let rec verifie ( lst  : ( int *  int )   list) : bool =
    match lst with
    | [] -> true 
    | [_] -> true  (*list with exactly one element*)
    | (a,b) :: (c,d) :: t -> (a+b <= c) && verifie ((c,d) :: t) (* a,b and c,d are the first and two element of the list t is the tail the rest of the list *)

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
    | [] -> [(addr, size)]
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

