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

let quartet_adder ((a1,a2,a3,a4):bool*bool*bool*bool) ((b1,b2,b3,b4):bool*bool*bool*bool) (c:bool) : (bool*bool*bool*bool)* bool=
    let ((s3 , s4), carry1 )= duet_adder (a3,a4) (b3,b4) c in
    let ((s1 , s2) , carry2)= duet_adder (a1,a2) (b1,b2) carry1 in
    ((s1 , s2 , s3 , s4 ) , carry2);;

let to_quartet (n : int) : bool * bool * bool * bool = 
    let b1 = (n/8) mod 2 = 1 in 
    let b2 = (n/4) mod 2 = 1 in 
    let b3 = (n/4) mod 2 = 1 in 
    let b4 = n mod 2 = 1 in 
    (b1,b2,b3,b4);;