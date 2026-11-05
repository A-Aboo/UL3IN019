type 'a btree = 
  | Empty
  | Node  of 'a * 'a btree * 'a btree


let t_ex = Node(5,
                 Node(3,
                       Node(4, Empty, Empty),
                       Node(5, Empty,Empty)),
                 Node(2, 
                       Node(3, 
                            Empty,
                            Node(7, Empty, Empty)),
                        Empty));;


let rec max_length_branch (t : 'a btree) : 'a list =
  match  t  with
  | Empty -> []
  | Node (v , g , d) ->
    if List.length( max_length_branch g  ) >= List.length(max_length_branch d) then
      v :: max_length_branch g 
    else 
      v :: max_length_branch d

let max_length_branch2 (t : 'a btree) : 'a list =
  let rec aux tr =
    match tr with
    | Empty -> ([], 0)
    | Node(v,g,d) ->
        let (bg,lbg) = aux g in
        let (bd,lbd) = aux d in
        if lbg >= lbd then
          (v :: bg, lbg + 1)
        else
          (v :: bd, lbd + 1)
  in
  let (branche, longueur) = aux t in
  branche

let rec max_length_branch3 (t : 'a btree) : 'a list =

  let rec aux a l s =
    match a with 
    | Empty -> (Empty, [],0)
    | Node (v,g,d) ->
      let (a1,lg,sg) = aux g l s in 
      let (a2,ld,sd) = aux d l s in 
      if sg >= sd then
        let sgg = sg +1 in
        (a1, v :: lg , sgg)
      else 
        let sdd = sd +1 in
        (a2 ,v :: ld, sdd)
    in
  let (a,l,s) =  aux t [] 0 in
  l
let rec max_flow_branch (t:'a btree) : 'a list=
    match t with
    | Empty -> []
    | Node (v,g,d) ->
      let s1 = max_flow_branch g in 
      let s2 = max_flow_branch d in 
      match s1 , s2 with 
      | [], [] -> v ::  s1 
      | [], _ -> v :: s2
      | _ , [] -> v :: s1 
      | h1::t1,h2::t2 -> 
        if h1 > h2 then
          v::h1::t1
        else 
          v::h2::t2


let rec at_prof (n : int) (x : 'a) (t : 'a btree) : bool =
  match t with
  | Empty -> false
  | Node (v , g, d ) ->
    if n = 0 && v = x then 
      true 
    else 
     if  at_prof (n-1) x g then true 
     else at_prof (n-1) x d 

let rec at_prof_list (n : int ) (t : 'a btree) : 'a list =
  match t with 
  | Empty -> []
  | Node (v , g , d ) -> 
    let lg = at_prof_list (n-1) g in
    let ld = at_prof_list (n-1) d in 
    if n = 0 then
      v :: (lg @ ld )
    else 
      lg @ ld 


(* 

let rec at_prof_list (n : int) (t : 'a btree) : 'a list =
  match t with
  | Empty -> []
  | Node (v, g, d) ->
      if n = 0 then
        [v]
      else if n < 0 then
        []
      else
        let lg = at_prof_list (n-1) g in
        let ld = at_prof_list (n-1) d in
        lg @ ld *)


let etiq_prof_list (x : 'a ) (t:'a btree) : int list =
  let rec helper a s =
    match a with 
    | Empty -> []
    | Node (v , g , d ) ->
      if v = x then 
        s::(helper g (s+1) @ helper d (s+1))
      else 
        (helper g (s+1)) @ (helper d (s+1))
    in

    helper (t) (0) 


let rec prof_max (t : 'a btree) : int =
  match t with 
  | Empty -> -1 
  | Node (v , g, d )->
    1 + max  ( prof_max g)  ( prof_max d) 


let max_prof_etiq (n:'a) (t : 'a btree) : int =
  List.fold_left(
    fun acc x -> 
      if x > acc then x else acc
  ) (-1) (etiq_prof_list n t)
  

let max_prof_etiq (n:'a) (t:'a btree) : int =
  List.fold_left max (-1) (etiq_prof_list n t)