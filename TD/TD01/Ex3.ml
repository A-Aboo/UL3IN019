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