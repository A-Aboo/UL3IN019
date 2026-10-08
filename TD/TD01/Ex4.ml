let test_match l m n =
  match l with
  | 0 -> m
  | 1 ->
      begin
        match m with
        | 0 -> n
        | _ -> m
      end
  | _ ->
      begin
        match m with
        | 0 -> l * n
        | 1 -> l + n
        | _ -> l + (2 * m) + n
      end

(*_* 

| m -> 

 <->

| _ ->

*___*)