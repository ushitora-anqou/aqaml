type ('a, 'b) t = ('a, 'b) Hashmap.t ref

let create size_hint = ref Hashmap.empty

let add tbl k v = tbl := Hashmap.add k v !tbl

let mem tbl k = Hashmap.mem k !tbl

let find tbl k = Hashmap.find k !tbl

let length tbl = Hashmap.cardinal !tbl
