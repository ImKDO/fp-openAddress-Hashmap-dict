module type DICT = sig
  type ('k, 'v) t

  val empty : unit -> ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  
  val find : 'k -> ('k, 'v) t -> 'v option
  
  val filter : ('k -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t
  val fold_left : ('acc -> 'k -> 'v -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc
  val fold_right : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

  val equal : ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool

  val append_dict : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t

end

module OaDict : DICT = struct
  type ('k, 'v) entry =
    | Empty
    | Deleted
    | Entry of 'k * 'v

  type ('k, 'v) t = {
    size : int; 
    capacity : int;
    entries : ('k, 'v) entry array;
  }

  let initial_capacity = 16
  let load_factor = 0.75

  let empty () = {
    size = 0;
    capacity = initial_capacity;
    entries = Array.make initial_capacity Empty;
  }

  let hash key capacity = (Hashtbl.hash key) mod capacity

  let rec find_entry_aux key entries capacity index count =
    if count >= capacity then None (* Предотвращение бесконечного цикла на заполненном массиве *)
    else
      match entries.(index) with
      | Empty -> None
      | Deleted -> find_entry_aux key entries capacity ((index + 1) mod capacity) (count + 1)
      | Entry (k, v) ->
          if k = key then Some v
          else find_entry_aux key entries capacity ((index + 1) mod capacity) (count + 1)

  let find key dict =
    if dict.size = 0 then None
    else find_entry_aux key dict.entries dict.capacity (hash key dict.capacity) 0

  let resize dict =
    let new_capacity = dict.capacity * 2 in
    let new_entries = Array.make new_capacity Empty in
    let add_entry_to_new (k, v) =
      let rec insert_aux index =
        match new_entries.(index) with
        | Empty | Deleted -> new_entries.(index) <- Entry (k, v)
        | Entry _ -> insert_aux ((index + 1) mod new_capacity)
      in
      insert_aux (hash k new_capacity)
    in
    Array.iter (function
      | Entry (k, v) -> add_entry_to_new (k, v)
      | _ -> ()
    ) dict.entries;
    { size = dict.size; capacity = new_capacity; entries = new_entries }

  let add key value dict =
    let dict' =
      if float_of_int (dict.size + 1) > float_of_int dict.capacity *. load_factor
      then resize dict
      else { dict with entries = Array.copy dict.entries }
    in
    let rec add_aux index =
      match dict'.entries.(index) with
      | Entry (k, _) when k = key -> (* Обновление *)
          dict'.entries.(index) <- Entry (key, value);
          dict'
      | Empty | Deleted -> (* Вставка *)
          dict'.entries.(index) <- Entry (key, value);
          { dict' with size = dict'.size + 1 }
      | Entry _ -> add_aux ((index + 1) mod dict'.capacity)
    in
    add_aux (hash key dict'.capacity)

  let insert = add

  let remove key dict =
    if dict.size = 0 then dict
    else
      let new_entries = Array.copy dict.entries in
      let rec remove_aux index count =
        if count >= dict.capacity then dict (* Ключ не найден *)
        else
          match new_entries.(index) with
          | Empty -> dict (* Ключ не найден, цепочка прервана *)
          | Entry (k, _) when k = key ->
              new_entries.(index) <- Deleted;
              { dict with size = dict.size - 1; entries = new_entries }
          | Entry _ | Deleted -> remove_aux ((index + 1) mod dict.capacity) (count + 1)
      in
      remove_aux (hash key dict.capacity) 0

  let fold_left f acc dict =
    Array.fold_left (fun a entry ->
      match entry with
      | Entry (k, v) -> f a k v
      | _ -> a
    ) acc dict.entries

  let fold_right f dict acc =
    Array.fold_right (fun entry a ->
      match entry with
      | Entry (k, v) -> f k v a
      | _ -> a
    ) dict.entries acc

  let filter p dict =
    fold_left (fun acc k v -> if p k v then add k v acc else acc) (empty ()) dict

  let equal value_eq d1 d2 =
    if d1.size <> d2.size then false
    else
      fold_left (fun are_equal k1 v1 ->
        are_equal &&
        match find k1 d2 with
        | Some v2 -> value_eq v1 v2
        | None -> false
      ) true d1

  let append_dict d1 d2 =
    fold_left (fun acc k v -> add k v acc) d2 d1
end 