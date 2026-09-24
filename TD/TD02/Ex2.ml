let rec verifie ( lst  : ( int *  int )   list) : bool =
    match lst with
    | [] -> true 
    | [_] -> true  (*list with exactly one element*)
    | (a,b) :: (c,d) :: t -> (c+d > a+b) && verifie ((c,d) :: t) (* a,b and c,d are the first and two element of the list t is the tail the rest of the list *)

