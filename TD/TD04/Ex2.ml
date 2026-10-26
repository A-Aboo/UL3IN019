type 'a etiquete = 
    | Elem of 'a * string


let creer (a : 'a ) ( s : string) :'a etiquete =
    Elem (a , s )

(* let cree a s = Elem(a,s) *)



let valeur (el : 'a etiquete) : 'a =
    let Elem (x,y) = el in 
    x 
(* let valeur (Elem (x,_)) = x  *)


let etiquette  ( el : 'a etiquete) : string =
    let Elem (x,y) = el in
    y 

(* let etiquete (Elem(_,s)) = s *)

let transformer_valeur (f : 'a -> 'b) (e : 'a etiquete) : 'b etiquete =
    match e with
    | Elem (a,b) -> 
        Elem ( (f a) ,b )
(*

let transformer_valeur (f : 'a -> 'b) (Elem(x , y)) : 'b etiquete =
    Elem ((f x) , y) 
    
*)


let etiqueter_liste (l : 'a list) (s : string) : 'a etiquete list =
    List.fold_right(
        fun x acc->
            Elem (x,s ) :: acc
    ) l []

    
(*
u want an easy approach !  
let etiqueter_liste2 ( l : 'a list) ( s : string) : 'a etiquete list =
        List.map(
            fun x -> 
                Elem (x,s)
        )l
masters !     
let etiqueter_liste (l : 'a list) (s : string ) : 'a etiquete list =
    match l with 
    | [] -> []
    | h :: t -> 
        Elem (h , s) :: etiqueter_liste t s  
*)

let changer_etiquette (s : string) (e : 'a etiquete) : 'a etiquete =
    match  e with
    | Elem (a,b) ->
        Elem (a , s)
        
(*
let changer_etiquette2 (s : string ) ( Elem(a,b)) : 'a etiquete =
    Elem (a , s)
 *)

let liste_animaux=["lion";"zebre";"girafe"]
let liste_references = [Elem(10,"");Elem(15,"");Elem(7,"")]
let notes = [Elem(4.5,"nom1"); Elem(15.4,"nom2"); Elem(10.8,"nom3");
             Elem(9.1,"nom4"); Elem(12.7,"nom5"); Elem(11.4,"nom6"); Elem(18.1,"nom7");
             Elem(16.2,"nom8")]


let iliste_animaux = 
    List.map(
        fun x ->
            Elem(x , "animal sauvage")
    ) liste_animaux

let iliste_references = 
    List.map(
        fun x ->
            changer_etiquette "en promotion" x
    ) liste_references


let changement_notation n =
  if (n < 10.) then "F" else
    if (n < 11.) then "E" else
      if (n < 12.) then "D" else
        if (n < 14.) then "C" else
          if (n < 16.) then "B" else
            if (n < 17.) then "A" else "A+"


let inotes = 
    List.map(
        fun x ->
            match x with
            | Elem (a,b) ->
                Elem (changement_notation a, b)
    ) notes

(* let inotes2 =
    List.map(
        fun x ->
            transformer_valeur changement_notation x
    ) notes *)

let verifie (f : 'a -> bool) (Elem (a,b)) : bool =
    f a


let rec separer p f1 f2 liste =
    match liste with
    | [] -> ([], [])
    | Elem(x, s) :: t ->
        let (l1, l2) = separer p f1 f2 t in
        if p x then
            (l1, Elem(f2 x, s) :: l2)
        else
            (Elem(f1 x, s) :: l1, l2)

let resultat =
    separer
        (fun x -> x >= 10.)
        (fun x -> "echec")
        (fun x -> x)
        notes