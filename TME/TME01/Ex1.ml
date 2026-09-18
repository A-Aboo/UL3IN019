

let rec sum_shiffre(n : int) : int = 
    if n < 10 then n 
    else sum_shiffre ( n / 10 ) + n mod 10;;

assert ((sum_shiffre 125) = 8);;
assert ((sum_shiffre 000) = 0);;
assert ((sum_shiffre 777) = 21);;

let rec nb_chiffre(n : int) : int =
    if n = 0 then 0
    else if n < 10 then 1
    else nb_chiffre (n / 10) + 1;;

assert ((nb_chiffre 125) = 3);;
assert ((nb_chiffre 000) = 0);;