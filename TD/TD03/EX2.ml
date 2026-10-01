type orientation = 
  | Nord  
  | Est 
  | Sud
  | Ouest


type position = {
    x : int ;
    y : int ;
}

type robot = {
    pos : position;
    dir : orientation;
}

let mon_robot : robot = {
    pos = {x =  4 ; y = 10};
    dir = Nord;
}

let deplacer ( n : int ) ( r : robot ) : robot =
  match r.dir with
  | Nord -> {r with pos = {x = r.pos.x; y = r.pos.y + n}}
  | Est -> {r with pos = {x = r.pos.x + n; y = r.pos.y}}
  | Sud -> {r with pos = {x = r.pos.x; y = r.pos.y - n}}
  | Ouest -> {r with pos = {x = r.pos.x - n; y = r.pos.y}}

let rotation ( r : robot ) : robot =
  match r.dir with
  | Nord -> {r with dir = Est}
  | Est -> {r with dir = Sud}
  | Sud -> {r with dir = Ouest}
  | Ouest -> {r with dir = Nord}

let rotation_n ( n : int ) ( r : robot ) : robot =
  let i = n mod 4 in
  let rec aux i r =
    if i <= 0 then r
    else aux (i - 1) (rotation r)
  in aux i r
