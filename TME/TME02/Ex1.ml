let rec lacunaire (l1 : 'a list) ( l2 : 'a list) : bool =
  match l1 with 
  | [] -> true 
  | h1 :: t1 -> 
    match l2 with 
    | [] -> false 
    | h2 :: t2 -> 
      if h1 = h2 then 
        lacunaire t1 t2
      else 
        lacunaire (h1::t1) (t2)

let rec sous_adj (l1 : 'a list ) ( l2 : 'a list) (n : 'a)  : bool =
    match l1 , l2 with
    | [] , []  -> true
    | [] , _ -> false
    | _ , [] -> false
    | h1::t1 , h2 :: t2 ->
      if h1 = n then 
        sous_adj t1 t2 n
      else if h1 = h2 then sous_adj t1 t2 n
       else false
    
let rec etendre (l1 : 'a list) (l2 : 'a list) (n : 'a) : 'a list =
  match l1 , l2 with 
  | [] , [] -> []
  | [] , _ :: t2 -> 
      n :: etendre [] t2 n
  | _::_ , [] -> failwith "Ahh shit !!! "
  | h1 :: t1 , h2 :: t2 ->
    if h1 = h2 then 
      h1 :: etendre t1 t2 n
    else 
      n :: etendre l1 t2 n


let rec helper (l1 : 'a list) ( l2 : 'a list) (n : 'a) : 'a list =
  match l1 , l2 with 
  | [], [] -> []
  | [] , _::_ -> failwith "Errorrrer"
  | _::_ , [] -> failwith "Errorrere"
  | h1 :: t1 , h2 :: t2 -> 
    if h1 = h2 then 
      h1 :: helper t1 t2 n
    else if h1 = n then 
        h2 :: helper t1 t2 n
    else if h2 = n then   
        h1 :: helper t1 t2 n
    else 
      failwith "unmatched numbers "

let rec fusion_lists (lxt : 'a list list ) (n : 'a) : 'a list =
  match lxt with 
  | [] -> []
  | [_] -> List.hd lxt 
  | h1 ::  t -> 
    helper h1 (fusion_lists t n  ) n 
