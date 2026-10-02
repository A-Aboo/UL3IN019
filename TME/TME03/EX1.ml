type valeur =
  | Deux
  | Trois
  | Quatre
  | Cinq
  | Six
  | Sept
  | Huit
  | Neuf
  | Dix
  | Valet
  | Dame
  | Roi
  | As

type couleur =
  | Coeur
  | Carreau
  | Trefle
  | Pique

type carte =
  | Carte of valeur * couleur
  | Joker

let  roipique = Carte (Roi, Pique)
let  ascoeur = Carte (As, Coeur)

let sup (c1 : carte) (c2 : carte) : bool =
  match (c1, c2) with
  | (Joker, Joker) -> false
  | (Joker, _) -> true
  | (_, Joker) -> false
  | (Carte (v1, _), Carte (v2, _)) -> v1 > v2

let egalite (c1 : carte) (c2 : carte) : bool =
  match (c1, c2) with
  | (Joker, Joker) -> true
  | (Joker, _) -> false
  | (_, Joker) -> false
  | (Carte (v1, _), Carte (v2, _)) -> v1 = v2