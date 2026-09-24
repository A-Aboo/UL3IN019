let rec repeat (n : int ) (e : 'a) : 'a list =
    match n with
    | 0 -> []
    | _ -> (e :: (repeat (n-1) e ))

  
(*
repeat 3 p
repeat 2 p 
repeat 1 p 
repeat 0 p
[]
[e]
[e;e]
[e;e;e]
[e;e;e;]

*)

let rec all_in_list (l1 : 'a list ) (l2 : 'a list) : bool =
    match l1 with 
    | [] -> true
    | h :: t -> (List.mem h l2 && (all_in_list t l2) ) ;;

(*
all_in_list [1,2] [1,3,4,5,6] 
all_in_list [1] [1,3,4,5,6]
all_in_list [] [1,3,4,5,6]
true 
true && true 
true && true && false 
val bool : false 

*)
let rec range_inter (a : int)  (b : int) : int list =
    if a > b then []
    else a :: (range_inter (a+1) b);;

