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

let rec soud_adj (l1 : 'a list ) ( l2 : 'a list) (n : 'a)  : bool =
    match l1 , l2 with
    | [] , []  -> true
    | [] , _ -> false
    | _ , [] -> false
    | h1::t1 , h2 :: t2 ->
      if h1 = n then 
        soud_adj t1 t2 n
      else if h1 = h2 then soud_adj t1 t2 n
       else false
    
let rec etendre (l1 : 'a list) (l2 : 'a list) (n : 'a) : 'a list =
  match l1 , l2 with 
  | [] , [] -> []
  | [] , _ :: t2 -> 
      n :: etendre [] t2 n
  | _::_ , [] -> failwith "Ahh shit !"
  | h1 :: t1 , h2 :: t2 ->
    if h1 = h2 then 
      h1 :: etendre t1 t2 n
    else 
      n :: etendre l1 t2 n