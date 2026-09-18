(* 
let xor (x : bool ) ( y :  bool ) : bool =
    if x && y then false
    else if x && not y then true 
    else if not x && y then true 
    else false ;;
*)

let xor (x : bool) (y : bool) : bool =
    x <> y;;

let half_adder (x : bool ) (y : bool) : bool * bool =
    ((xor x y )  , x && y );;

let adder (a : bool ) (b : bool ) ( c : bool) : bool * bool =
    let (s1 , r1 ) = half_adder a b in
    let (s2 , r2 ) = half_adder c s1 in 
    ( s2 , (r1 || r2) );; 

let duet_adder ( (a1 , a2) :  bool * bool  )  (  (b1 , b2) : bool * bool )  (c1 : bool) : (bool * bool) * bool = 
    let (s2 , r2 ) = adder a2 b2 c1 in 
    let (s1 , r1 ) = adder a1 b1 r2 in 
    (  (s1 , s2 ), r1)

