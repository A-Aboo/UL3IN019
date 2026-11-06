type 'a htree =
| Leaf of int * 'a
| Branch of int * 'a htree * 'a htree

let t_msg = Branch(17,
                  Leaf(8,'A'),
                  Branch(9
                          ,Branch(4,Branch(2,Leaf(1,'C'),Leaf(1,'G')),
                                  Branch(2,Leaf(1,'H'),Leaf(1,'F'))),
                  Branch(5,Branch(2,Leaf(1,'E'),Leaf(1,'D')),Leaf(3,'B'))))
let msg = ['A';'A';'B';'A';'C';'B';'A';'G';'H';'A';'A';'F';'E';'A';'D';'B';'A']
let msg_01 = [0;0;1;1;1;0;1;0;0;0;1;1;1;0;1;0;0;1;1;0;1;0;0;0;1;0;1;1;1;1;0;0;0;1;1;0;1;1;1;1;0]

let t1 = Leaf(3,'B')
let t2 = Branch(5,Leaf(2,'Z'),Leaf(3,'B'))
let t3 = Branch(5,Leaf(2,'Z'),Leaf(3,'W'))
let t4 = Leaf(3,'K')

let l1 = [Leaf(8,'A');Leaf(3,'B');Leaf(1,'D');Leaf(1,'E')
          ;Leaf(1,'F');Leaf(1,'H');Leaf(1,'G');Leaf(1,'C')]
let l2 = [('A',8);('B',3);('D',1);('E',1);('F',1);('H',1);('G',1);('C',1)]

let rec helper (l :('a * int list ) list ) (n:int) : (('a * int list ) list ) =
  match l with 
  | [] -> []
  | h::t ->
    match h with
    | (c,l) ->
      [ (c, n::l)] @ helper t n 



let rec huff_tab (t : 'a htree) : ('a * int list) list =
  match t with
  | Leaf (c, h) -> [(h, [])]
  | Branch (v, g, d) ->
    let lg = huff_tab g in 
    let ld = huff_tab d in 
    (helper lg 0)  @ (helper ld 1)
    
(* let rec huff_tab (t : 'a htree) : ('a * int list) list =
  match t with
  | Leaf (_, c) -> [(c, [])]
  | Branch (_, g, d) ->
      let lg = huff_tab g in
      let ld = huff_tab d in
      List.map (fun (c, l) -> (c, 0 :: l)) lg
      @
      List.map (fun (c, l) -> (c, 1 :: l)) ld *)

let rec get_code ( c: ('a * int list) list) (s : 'a ) : int list =
  match c with 
  | [] -> []
  | (c,l)::t ->
    if c = s then l 
    else get_code t s

let rec code (m:'a list) ( c: ('a * int list) list) : int list =
  match m with 
  | [] -> []
  | h :: t ->
    let l = get_code c h in   
    if List.for_all (fun x -> x = 0 || x = 1) l then
      l @ code t c
    else
      failwith "Nigga binary  <> 0-1 :( "
(*       
let rec code (m : 'a list) (c : ('a * int list) list) : int list =
  match m with
  | [] -> []
  | h :: t ->
      let l = List.assoc h c in
      if List.for_all (fun x -> x = 0 || x = 1) l then
        l @ code t c
      else
        failwith "Code Huffman invalide" *)

let decode1 (l: int list) (t : 'a htree) : 'a * int list =
  let rec helper acc (li:int list) =
    match li with 
    | [] -> failwith "Code incomplet"
    | h::ta ->
      let new_acc = acc @ [h] in 
      match  List.find_opt (fun (c,ll) -> ll = new_acc) (huff_tab t) with 
      | None -> helper new_acc ta 
      | Some(v,lv) -> (v ,ta)
      in
      helper [] l 


(*

let rec decode1 (l : int list) (t : 'a htree) : 'a * int list =
  match t, l with
  | Leaf (_, c), _ -> (c, l)
  | Branch (_, g, d), 0 :: reste -> decode1 reste g
  | Branch (_, g, d), 1 :: reste -> decode1 reste d
  | _ -> failwith "Code invalide ou incomplet" 
  
  
  *)


let rec decode (l : int list) (tr : 'a htree) : 'a list =
  match l with 
  | [] -> []
  | h :: t ->
    let (v,reste) = decode1 (h::t) tr in 
    v :: decode reste tr 

let freq_ht (t : 'a htree) : int =
  match t with
  | Leaf (f , v ) -> f
  | Branch(v , g , d) -> v

let ht_less (t1: 'a htree) (t2 : 'a htree) : bool =
  if freq_ht t1 < freq_ht t2 then true
  else false 


let min_sauf_min (lt : 'a htree list) : 'a htree * 'a htree list =
  let rec helper tmin ltree = 
    match ltree with 
    | [] -> failwith "Really, Empty List !!!"
    | [h] -> (h,[])
    | h :: t ->
      let (i,j) = helper tmin t in 
      if ht_less h i then 
        h , i :: j 
    else 
      i , h :: j 
    in 
    helper (List.hd lt ) lt   

let ht_branch (t1 : 'a htree) ( t2 : 'a htree) : 'a htree =
  Branch (    
    freq_ht t1 + freq_ht t2 ,
     t1 ,
     t2 
  )

let rec make_huff (l  :'a htree list) : 'a htree =
  match l with 
  | [] -> failwith "Naaaa"
  | [h] -> h
  | h :: t ->
    let l1 = (h::t) in 
    let (m1 , lres1) = min_sauf_min (l1) in 
    let (m2, lres2) = min_sauf_min (lres1) in 
    let deux = ht_branch m1 m2 in 
    make_huff (deux::lres2)

let rec leaf_list (f : ('a * int ) list) : 'a htree list =
  match f with 
  | [] -> []
  | (a,b) ::t ->
    Leaf (b,a) :: leaf_list t 


let huff_of_freq (f : ('a * int) list) : 'a htree =
  make_huff ( leaf_list f )

let rec freq (l : 'a list) : ('a* int ) list =
  match l with 
  | [] -> []
  | [h] -> [(h,1)]
  | h :: t -> 
    let size = List.length (
      List.filter (
        fun x -> 
          x  = h 
      ) t 
    ) in
    let lres = List.filter (fun x -> x <> h ) t in 
    (h , size+1) :: freq lres

let make_code (m : 'a list) : (int list ) * 'a htree =
  let freqq = freq m in 
  let tree = huff_of_freq freqq in 
  let cod = code m  ( huff_tab tree ) in 
  cod,tree 