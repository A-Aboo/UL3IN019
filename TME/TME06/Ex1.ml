type eleve = string * int * float option
type base = eleve list

exception Base_vide

let ma_base = [("nom1",245,Some 10.);("nom2",44,Some 15.5);("nom23",12,Some 7.6);("nom4",100,None)]
let base_sans_note = [("nom1",245,None);("nom2",44,None);("nom23",12,None);("nom4",100,None)]

let num_eleve (a,b,c : eleve) = 
 b
let note_eleve eleve = 
  let (_,_,c) = eleve in 
  c

let nom_eleve eleve = 
  match eleve with 
  | (a,_,_) -> a
let rechercher_opt num b =
  List.find_opt( fun (a,b,c) -> b = num ) b

let rec rechercher num b =
  match b with 
  | [] -> None
  | (a,b,c)::t ->
    if b = num then Some (a,b,c)
    else rechercher num t 

let maj_note num note b =
  if  not ( List.exists (fun (a,b,c) -> b = num) b  ) then None
  else Some( List.map(
    fun (a,b,c) -> 
      if b = num then (a,b, Some note)
      else (a,b,  c)
  ) b)


let maj_si_existe num note b =
  if (rechercher num b) <> None then 
    match maj_note num note b with
    | None -> b
    | Some new_base -> new_base
  else b

let rec ajouter nom num b = 
  match rechercher_opt num b with 
  | Some (_,_,_) -> b
  | None ->  b @ [(nom,num ,None)]

let rec ajouter2 nom num b = 
  match b with 
  | [] -> [(nom,num, None)]
  | (a,bb,c)::t ->
    if bb = num then [(a,bb,c)] @ t
    else [(a,bb,c)] @ ajouter2 nom num t
let rec meilleure_note b =
  if b = [] then raise Base_vide  
  else if List.for_all(fun (a,b,c) ->c = None) b then None
  else Some( List.fold_left max 0. ( List.filter_map (fun (_,_,c) -> c) b) )

let meilleure_note b =
  if b = [] then raise Base_vide
  else List.fold_left (
    fun acc (_,_,c) -> max acc c
  ) None b

let rec meilleure_note_liste bl =
  if bl = [] then raise Base_vide
  else
     List.fold_left max None  (List.fold_left(fun acc x -> 
      if x = [] then acc else 
      (meilleure_note x) :: acc ) [] bl) 
  

let meilleure_note_liste bl =
  if bl = [] then raise Base_vide
  else List.fold_left (
    fun acc x ->
      if x = [] then acc
      else max acc (meilleure_note x)
  ) None bl


let  moyenne_presents b =
   let rec helper l sum n = 
    match l with
    | [] -> if n = 0. then None 
            else Some ( sum /. n )
    | (_,_,None ) :: t -> helper t sum n
    | (_,_,Some c ) :: t -> helper t (sum +. c) (n +. 1.)
          in
    helper b 0. 0.   


let  moyenne_promo b =
  let rec helper l sum n =
  match l with
  | [] -> if n =0. then raise Base_vide
          else  (sum /. n)
  | (_,_,None):: t -> helper t sum (n +. 1.)
  | (_,_,Some c)::t-> helper t (sum +. c) (n +. 1.)
        in
        
  helper b 0. 0.

let rechercher_prefixe prefix b =
 List.fold_left (
    fun acc (a,b,c) -> 
      if  (String.sub a 0 (String.length prefix)) = prefix then acc @[(a,b,c)]
      else acc

 ) [] b

let rechercher_prefixe prefix b =
  List.filter (
    fun (a,_,_) -> 
      String.sub a 0 (String.length prefix) = prefix
  ) b
