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

let compare (c1 : carte ) ( c2 :carte ): int =
    match c1 , c2 with 
    | Joker , Joker -> 0
    | _ , Joker -> -1 
    | Joker , _ -> 1 
    | _ , _ -> 
        if egalite c1 c2 = true then 0 
        else if sup c1 c2 = true then 1 
        else -1 


let list_valeur = [ Deux ; Trois ; Quatre ; Cinq
; Six ; Sept ; Huit ; Neuf ; Dix ; Valet ; Dame ; Roi ; As ]

let list_color = [ Pique ; Coeur  ; Trefle ; Carreau] 

let genere_couleur (l : couleur list) ( v : valeur ) : carte list =
    List.fold_left (
        fun acc x ->
        Carte (v ,x)::acc
    )[] l

let rec genere_coul (l: couleur list) (v : valeur ) : carte list =
    match l with
    | [] -> []
    | h::t -> 
        Carte (v,h ) :: genere_coul t v 

let genere_paquet ( ) = 
    List.fold_left (
        fun acc x ->
            genere_couleur list_color x @ acc
    ) [] list_valeur

let rec n_ieme (l: 'a list) (n :int ) : 'a * 'a list =
    let i = List.nth l n in 
    (i , List.filter( fun x -> x <> i ) l)

let rec neme (l : 'a list) (n : int ) : 'a * 'a list =
    match l with 
    | [] -> failwith "EMPAATYY LIIST"
    | h :: t -> 
        if n = 0 
            then h , t 
        else 
            let (x, reste) = neme t (n-1 ) in 
            (x , h :: reste)

let rec distribution (p : carte list) (taille : int) (nb : int) : carte list * carte list =
    if nb = 0 
        then ([], p)
    else 
        let index = Random.int taille in 
        let (x,y) = neme p index in 

        let ( a  , b  ) = distribution y (taille-1) (nb-1) 
        in
        (x :: a , b )

let bataille (p1 : carte list) (p2 : carte list) (n : int) : carte list * carte list =

  let rec aux p1 p2 tour gagnees =
    if tour = n then
      (p1, p2)
    else
      match p1, p2 with
      | [], _ -> (p1, p2)
      | _, [] -> (p1, p2)
      | c1 :: r1, c2 :: r2 ->

          if compare c1 c2 = 1 then
            aux(r1 @ gagnees @ [c1; c2]) r2(tour + 1)[]
          else if compare c1 c2 = -1 then
            aux r1(r2 @ gagnees @ [c1; c2])(tour + 1)[]
          else aux r1 r2 tour(gagnees @ [c1; c2])
        in

  aux p1 p2 0 []

let jouer () : carte list * carte list =
  let paquet = genere_paquet () in
  let (p1, p2) = distribution paquet 54 27 in
  bataille p1 p2 1000