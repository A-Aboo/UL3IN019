type 'a score =  string * 'a * string


let creer_score (j:string) (s : 'a) (ju:string) : 'a score =
   (j , s , ju)


let joueur (s : string * 'a * string ) : string =
  let (a,b,c) = s in 
  a 

let joueur (a, b, c) = a

let joueur ((a, b, c) : 'a score) : string =
    a
(* same for the rest ! ! *)
let valeur_score (a,b,c) = b

let jeu ((a,b,c) : 'a score) : string = c

let changer_score (f : 'a -> 'a ) ((a,b,c) : 'a score) : 'a score =
  creer_score ( a)  (f b) ( c  )

let rec liste_scores (l : (string * 'a) list  ) (s :string) : 'a score list =
  match l with 
  | [] -> []
  | (a,b)::t ->
    creer_score a b s :: liste_scores t s

let scores_cartes : int score list =
    [("Ali", 10, "Cartes");
     ("Sara", 15, "Cartes");
     ("Tom", 20, "Cartes")]

let scores_golf : int score list =
    [("Ali", 5, "Golf");
     ("Sara", 8, "Golf");
     ("Tom", 12, "Golf")]

let scores_courses : float score list =
    [("Ali", 12.5, "Courses");
     ("Sara", 10.8, "Courses");
     ("Tom", 15.2, "Courses")]

let meilleur_score (f : 'a -> 'a -> int) (a : 'a) (l : 'a score list) : 'a =
    match l with
    | [] -> a
    | (j, s, jeu) :: t ->
        List.fold_left (
            fun acc (j, x, jeu) ->
                if f x acc > 0 then x
                else acc
        ) s t

let compare_inv x y = compare y x
let meilleur_score7 = meilleur_score

let meilleur_score (f : 'a -> 'a -> int) (a : 'a)
    (jeu : string) (l : 'a score list) : 'a =

    let l2 = List.filter (
        fun (j, s, nom) ->
            nom = jeu
    ) l in

    meilleur_score7 f a l2

(* let liste_mixte =
    [("J1", 14, "cartes");
     ("J1", 80, "golf");
     ("J2", 20, "cartes");
     ("J2", 160, "golf")]

let test1 = meilleur_score compare (-1) "cartes" liste_mixte
(* 20 *)

let test2 = meilleur_score compare_inv (-1) "golf" liste_mixte
(* 80 *)

let test3 = meilleur_score compare (-1) "tennis" liste_mixte
-1 *)