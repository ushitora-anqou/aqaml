open Printf

let id_counter = ref 0

let make_id base =
  id_counter := !id_counter + 1 ;
  sprintf "%s.%d" base !id_counter

let hashmap_of_list src =
  let hashmap = ref Hashmap.empty in
  List.iter (fun (k, v) -> hashmap := Hashmap.add k v !hashmap) src ;
  !hashmap

let integrate od nw = Hashmap.union (fun _ _ r -> Some r) od nw
