type nom =
    | Blinky 
    | Pinky 
    | Inky 
    | Clyde 

type couleur = 
    | Rouge 
    | Rose 
    | Bleu 
    | Orange 

type position = {
    x : int ;
    y : int ;
}


type fantome = {
    pos : position;
    no : nom;
    col : couleur;
}

let pinky : fantome = {
    pos = {x =  0 ; y = 0};
    no = Pinky;
    col = Rose;
}

let dep_aleatoire pacman fantome =
  fantome
let dep_vers_pacman pacman fantome =
  fantome

let  dep_devant_pacman pacman fantome =
  fantome

let dep_bizarre pacman fantome =
  fantome

let deplace (pos : position) fantome =
  match fantome with
  | { no = Blinky; _ } -> dep_aleatoire pos fantome
  | { no = Pinky; _ } -> dep_vers_pacman pos fantome
  | { no = Inky; _ } -> dep_devant_pacman pos fantome
  | { no = Clyde; _ } -> dep_bizarre pos fantome

let deplace_tous (pos: position) (fantomes : fantome list) : fantome list =
  List.map (deplace pos) fantomes