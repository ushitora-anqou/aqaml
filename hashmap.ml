type ('a, 'b) t = ('a * 'b) list

let empty = []

let add k v m = (k, v) :: m

let rec find k = function
  | (k', v') :: xs -> if k = k' then v' else find k xs
  | [] -> raise Not_found

let mem k m =
  try
    ignore (find k m) ;
    true
  with Not_found -> false

let merge f m1 m2 =
  let src = ref empty in
  let rec iter_m1 = function
    | (k, v) :: xs ->
        ( try src := add k (Some v, Some (find k m2)) !src with Not_found ->
            src := add k (Some v, None) !src ) ;
        iter_m1 xs
    | [] -> ()
  in
  let rec iter_m2 = function
    | (k, v) :: xs ->
        if not (mem k m1) then src := add k (None, Some v) !src ;
        iter_m2 xs
    | [] -> ()
  in
  iter_m1 m1 ;
  iter_m2 m2 ;
  List.fold_left
    (fun m (k, (l, r)) -> match f k l r with None -> m | Some v -> add k v m)
    empty !src

let union f m1 m2 =
  merge
    (fun k l r ->
      match (l, r) with
      | None, None -> None
      | Some v, None -> l
      | None, Some v -> r
      | Some v1, Some v2 -> f k v1 v2 )
    m1 m2

let cardinal m = List.length m
