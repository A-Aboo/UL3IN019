(* *)
(* fonction pour tester la lev�e d'une exception 
 'assert_raises exn f' :  v�rifie que 'f ()' l�ve bien l'exception 'exn'.
 *)
let assert_raises expected_exn f =
  match f () with
  | _ -> assert false  (* aucune exception lev�e : �chec *)
  | exception e -> if e <> expected_exn then failwith "exception inattendue"


(* exercice 1 *)
    
type eleve = string * int * float option
type base = eleve list

exception Base_vide

let ma_base = [("nom1",245,Some 10.);("nom2",44,Some 15.5);("nom23",12,Some 7.6);("nom4",100,None)]
let base_sans_note = [("nom1",245,None);("nom2",44,None);("nom23",12,None);("nom4",100,None)]

let num_eleve (a,b,c : eleve) = 
 b

let () = assert(num_eleve ("nom1",245,Some 10.) = 245)

let note_eleve eleve = 
  let (_,_,c) = eleve in 
  c

let () = assert(note_eleve ("nom1",245,Some 10.) = Some 10.)

let nom_eleve eleve = 
  match eleve with 
  | (a,_,_) -> a

let () = assert(nom_eleve ("nom1",245,Some 10.) = "nom1")

(* version qui utilise List.find_opt *)
let rechercher_opt num b =
  List.find_opt( fun (a,b,c) -> b = num ) b

let () = assert(rechercher_opt 245 ma_base = Some ("nom1", 245, Some 10.))
let () = assert(rechercher_opt 200 ma_base = None)

(* version sans utiliser List.find_opt *)
let rec rechercher num b =
  match b with 
  | [] -> None
  | (a,b,c)::t ->
    if b = num then Some (a,b,c)
    else rechercher num t 

let () = assert(rechercher 245 ma_base = Some ("nom1", 245, Some 10.))
let () = assert(rechercher 200 ma_base = None)
  
let maj_note num note b =
  if  not ( List.exists (fun (a,b,c) -> b = num) b  ) then None
  else Some( List.map(
    fun (a,b,c) -> 
      if b = num then (a,b, Some note)
      else (a,b,  c)
  ) b)

let () = assert (maj_note 245 12. ma_base = Some [("nom1", 245, Some 12.); ("nom2", 44, Some 15.5); ("nom23", 12, Some 7.6); ("nom4", 100, None)])
let () = assert (maj_note 200 12. ma_base = None)
    
let maj_si_existe num note b =
  if (rechercher num b) <> None then 
    match maj_note num note b with
    | None -> b
    | Some new_base -> new_base
  else b

let () = assert (maj_si_existe 100 15. ma_base = [("nom1", 245, Some 10.); ("nom2", 44, Some 15.5); ("nom23", 12, Some 7.6); ("nom4", 100, Some 15.)])
let () = assert (maj_si_existe 50 11.5 ma_base = [("nom1", 245, Some 10.); ("nom2", 44, Some 15.5); ("nom23", 12, Some 7.6); ("nom4", 100, None)])

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

let () = assert (ajouter "nom2" 50 ma_base = [("nom1", 245, Some 10.); ("nom2", 44, Some 15.5); ("nom23", 12, Some 7.6); ("nom4", 100, None); ("nom2", 50, None)])
let () = assert (ajouter "nom1" 44 ma_base = [("nom1", 245, Some 10.); ("nom2", 44, Some 15.5); ("nom23", 12, Some 7.6); ("nom4", 100, None)])
            
let rec meilleure_note b =
  if b = [] then raise Base_vide  
  else if List.for_all(fun (a,b,c) ->c = None) b then None
  else Some( List.fold_left max 0. ( List.filter_map (fun (_,_,c) -> c) b) )

let meilleure_note b =
  if b = [] then raise Base_vide
  else List.fold_left (
    fun acc (_,_,c) -> max acc c
  ) None b

let () = assert_raises Base_vide (fun () -> meilleure_note [])
let () = assert (meilleure_note ma_base = Some 15.5)
let () = assert (meilleure_note [("nom1",245,None);("nom2",44,None)] = None)

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


let () = assert_raises Base_vide (fun () -> meilleure_note_liste [])
let () = assert (meilleure_note_liste [ma_base;[("nom3",4,None);("nom4",45,Some 26.4)]] = Some 26.4)
let () = assert (meilleure_note_liste [ma_base;[]] = Some 15.5)


let  moyenne_presents b =
   let rec helper l sum n = 
    match l with
    | [] -> if n = 0. then None 
            else Some ( sum /. n )
    | (_,_,None ) :: t -> helper t sum n
    | (_,_,Some c ) :: t -> helper t (sum +. c) (n +. 1.)
          in
    helper b 0. 0.   

let () = assert (moyenne_presents ma_base = Some 11.0333333333333332)
let () = assert (moyenne_presents [] = None)
  
let  moyenne_promo b =
  let rec helper l sum n =
  match l with
  | [] -> if n =0. then raise Base_vide
          else  (sum /. n)
  | (_,_,None):: t -> helper t sum (n +. 1.)
  | (_,_,Some c)::t-> helper t (sum +. c) (n +. 1.)
        in
        
  helper b 0. 0.


let () = assert (moyenne_promo ma_base = 8.275)
let () = assert_raises Base_vide (fun () -> moyenne_promo [])
let () = assert (moyenne_promo [("nom1",245,None);("nom2",44,None)] = 0.)

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

let () = assert (rechercher_prefixe "nom2" ma_base = [("nom2", 44, Some 15.5); ("nom23", 12, Some 7.6)])
let () = assert (rechercher_prefixe "name" ma_base = [])

