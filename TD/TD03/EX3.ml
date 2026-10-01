type valeur = 
  | Singleton of int
  | Couple of int * int



let ma_liste =
  [Singleton(4);Couple(5,6);Couple(-1,4);Singleton(2)]

let to_couple (v : valeur list) : valeur list =
  List.map (function
    | Singleton(x) -> Couple(x,0)
    | Couple(x,y) -> Couple(x,y)) v   

let rec somme ( v : valeur list) : int =
  match v with
  | [] -> 0
  | Singleton(x)::q -> x + somme q
  | Couple(x,y)::q -> x + y + somme q
  
let somme ( v : valeur list) : int =
  List.fold_left (fun acc x -> match x with
    | Singleton(x) -> acc + x
    | Couple(x,y) -> acc + x + y) 0 v