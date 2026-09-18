
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