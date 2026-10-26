type 'a btree = 
  | Empty
  | Node  of 'a * 'a btree * 'a btree

type 'b arbrebinaire = 
  | Vide
  | Noeud of 'b arbrebinaire * 'b * 'b arbrebinaire

let t_ex = Node(5,
                 Node(3,
                       Node(4, Empty, Empty),
                       Node(5, Empty,Empty)),
                 Node(2, 
                       Node(3, 
                            Empty,
                            Node(7, Empty, Empty)),
                        Empty));;

let a_ex = Noeud( 
                 Noeud(
                        Noeud(Vide, 4, Vide)
                        , 3,
                        Noeud(Vide, 5, Vide)),
                  5, 
                 Noeud(
                        Noeud(Vide
                              , 3, 
                              Noeud(Vide, 7, Vide))
                        , 2,
                        Vide));;

let rec somme_etiq_ang (t : 'a btree) : int =
  match t with 
  | Empty -> 0
  | Node (e , g , d ) ->
    e + somme_etiq_ang g + somme_etiq_ang d

let rec somme_etiq_fr (t : 'b arbrebinaire) : int =
  match t with 
  | Vide -> 0
  | Noeud (g , v , d ) -> 
    v + somme_etiq_fr g + somme_etiq_fr d  


let rec egalite (t1 : 'a btree )(t2 : 'a arbrebinaire) : bool =
  match t1 , t2 with
  | Empty , Vide -> true
  | Empty , _ -> false 
  | _ , Vide -> false 
  | Node(v1,g1,d1) , Noeud(g2,v2,d2) ->
    v1 = v2 && egalite g1 g2 && egalite d1 d2 


type 'a arbre = 
  | Btree of  'a btree
  | Arbre of  'a arbrebinaire

let rec somme_etiq (t : int arbre list) : int =
  match t with
  | [] -> 0
  | Btree(a)::reste -> somme_etiq_ang a + somme_etiq reste
  | Arbre(a)::reste -> somme_etiq_fr a + somme_etiq reste

let somme_etiq (t : int arbre list ) : int =
   List.fold_left(
      fun acc x  ->
        match x with
        | Btree (a)-> somme_etiq_ang a + acc
        | Arbre (a) -> somme_etiq_fr a + acc
  ) 0 t  