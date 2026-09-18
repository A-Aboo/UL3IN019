
let rec plus_petite_diviseur(i : int ) (n : int) : int = 
    if i = n then 0 
    else if n mod i = 0 then i 
    else plus_petite_diviseur (i+1) n;;

let premier (n : int) : bool = 
    if n = 1 then false 
    else 
        let rec check i =    
            if i = n  then true
            else if n mod i = 0 then false 
            else check (i +1)
            in
    check 2

let premier_sup (n : int ) : int =
    if premier n then n 
    else 
        let rec check i = 
            if premier(i) then i 
            else check ( i  +1 ) 
        in
    check n;;

let rec premier_pos (n : int) : int =
    if premier n then premier_sup 0 
    else 
        let rec check (i : int) (j : int) : int = 
            if j = n then i 
            else premier_sup(i) 
        in 
    check 2 0 ;;