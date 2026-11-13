type expr = 
  | Const of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 


type 'a option =
  | None 
  | Some of 'a
  
exception Division_par_zero
exception Division_par_zero_avec of expr


let rec eval (e : expr ) : int =
  match e with 
  | Const (i) -> i
  | Add (a,b) -> eval(a) + eval(b)
  | Sub (a,b) -> eval(a) - eval(b)
  | Mul (a,b) -> eval(a) * eval(b)
  | Div (a,b) -> if eval(b) = 0 then raise (Division_par_zero_avec b) 
  else eval(a)/eval(b)

(* if eval(b) = 0 then raise Division_par_zero  *)

let safe_eval (e:expr) : int option = 
  try 
    Some (eval e)
  with
    | Division_par_zero_avec x-> None

let racine_option (f : float) : float option =
  if f >= 0. then (Some (sqrt f))
  else None

let div_option (x : float) (y :float) : float option =  
  if y = 0. then None
  else Some (x/.y)

let bind_option (s: 'a option) (f : 'a -> 'b option) : 'b option =
  match s with
  | None -> None 
  | Some x -> f x 

let compose_option (f1 : 'a -> 'b option) (f2:'b -> 'c option) (x:'a):'c option=
  match f1 x with
  | None -> None
  | Some y -> f2 y 

let compose_option (f1 : 'a -> 'b option) (f2:'b -> 'c option) (x:'a):'c option=
  bind_option (f1 x) f2 

let div_racine_option (x : float ) (y : float) : float option =
  compose_option 
    (fun a -> div_option a y)
    racine_option 
    x

let rec filter_option (l : 'a option list) : 'a list =
  match l with
  | [] -> []
  | None :: t -> filter_option t 
  | Some x:: t -> x :: filter_option t 

let filter_option2 (l : 'a option list) : 'a list =
  List.fold_left(
    fun acc x ->
      match x with 
      | None -> acc
      | Some y ->  acc @ [y]
  ) [] l
(* if 
  y :: acc  
  then res =  [2 ; 5 ; 4 ]
  *)
  
let rec map_filter (f : 'a -> 'b option) (l:'a list) : 'b list =
  match l with 
  | [] -> []
  | h :: t ->
    match f h with 
    | None -> map_filter f t
    | Some x -> x :: map_filter f t

let map_filter2 (f:'a -> 'b option) (l : 'a list) : 'b list =
  let ll = List.map( fun x -> f x) l in 
  filter_option ll 

let racine_liste (l : float list) : float list =
  map_filter racine_option l 