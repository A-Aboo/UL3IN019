  let est_pair (n : int ) : bool=
    n mod 2 = 0;;

  (* let rec syracuse (ns : int ) (k : int ) : int =
    if k = 0 then ns
    else 
      let rec check i u= 
        if i = k then u
        else if est_pair(u) then check ( i + 1) (u / 2)
        else check (i+1) (3 * u +1)
      in
    check 0 ns;;  *)

let rec syracuse (ns : int ) (k : int) : int = 
  if k = 0 then ns
  else let u = syracuse ns (k-1) in
    if est_pair ( u) then
      u/2 
    else 3 * u + 1
  ;;



let rec syracus2 (ns : int) (k : int) : int =
  match ns,k with 
  | _, 0 -> ns 
  | ns , _ when est_pair ns -> syracus2 (ns/2) (k-1)
  | _ , _ ->  syracus2  (3 * ns + 1) (k-1) 

let rec syracus3 (ns : int ) (k : int ) : int =
  if k = 0 then ns
    else if est_pair ns then
      syracus3 (ns /2 ) (k-1)
    else syracus3 (3*ns + 1) (k-1)


let altitude_max (f : int -> int) (k : int) : int =
  let rec loop i maxi =
    if i > k then
      maxi
    else
      let valeur = f i in
      if valeur > maxi then
        loop (i + 1) valeur
      else
        loop (i + 1) maxi
  in
  loop 1 (f 0)

let applique_test (test : int -> bool) (ns : int) : int =
  let rec loop n =
    if test (syracus ns n) then
      n
    else
      loop (n + 1)
  in
  loop 0

let test_vol (n : int) : bool =
  n = 1
let test_altitude (m : int) (a : int) : bool =
  a < m;;
