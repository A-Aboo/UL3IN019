
let films =
[("Le comte de Monte Cristo", [("Delaporte","Matthieu");("De LaPatelliere","Alexandre")],2024,[("Niney","Pierre");("Bouillon","Bastien");("Demoustier","Anais")])
;
("Le Temps D’Aimer", [("Quillevere","Katell")],2023,[("Demoustier","Anais");("Lacoste","Vincent");("Beaurepaire","Paul")]);

("Yves Saint-Laurent", [("Lespert","Jalil")],2014,[("Niney","Pierre");("Gallienne","Guillaume");("LeBon","Charlotte")]);

("Falcon Lake", [("Le Bon","Charlotte")],2022,[("Engel","Joseph");("Montpetit","Sara");("Chokri","Monia")]);

("Un p’tit truc en plus", [("Artus","")],2024,[("Artus", "");("Cornillac","CLovis");("Belaide","Alice")]);
]


(*
typo in the first test "LeBon" and not "Le Bon" !!! 
*)

type t_film = (string * (string * string) list * int * (string * string) list) list

let list_titres (film : t_film) : string list = 
    List.map (fun (x , _ , _ , _) -> x) film

let filme_annee (film : t_film) (n : int): string list =
    list_titres (   List.filter (  fun (_ , _ , x ,  _ ) -> x =  n) film   )


let filme_annee2 (film : t_film ) (n : int) : string list =
    List.fold_left   (

        fun acc (titre , _ , annee , _ ) -> 
            if annee = n then
                acc @ [titre]
            else 
                acc

    ) [] film

let films_interprete (prenom : string) (nom : string) (films : t_film ) : string list =
    list_titres
        (List.filter
            (fun (_, _, _, acteurs) -> List.mem (nom, prenom) acteurs)
            films)