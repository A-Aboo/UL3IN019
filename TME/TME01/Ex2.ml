Random.self_init ();;

let lancer_de () : int =
    Random.int (6 -1 + 1 )+ 1;;

let x = lancer_de() in 
assert (x >= 1 && x <= 6);;

let tour (n :int ) : int =
    let rec three (i : int ) : int =
      if i = 0 then 0 
      else lancer_de() + three(i -1) in 
    let x = three 3 in 
    if x > n then n 
    else n - x;;

let jouer (a : int ) (b : int ) (c : int ) : int * int * int = 
    let rec turn (x : int ) (y : int ) ( z : int ) : int * int *int =

        if x = 0 || y = 0 || z = c then (x , y , z)

        else turn  (  tour x )  ( tour y )   ( z + 1  ) in 

    turn a b 0;;

let jouer_501 (n : int) : int * int * int =
    jouer 501 501 n ;;