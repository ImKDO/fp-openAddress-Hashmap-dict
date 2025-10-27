module D = Fp_openAddress_Hashmap_dict.Dict.OaDict

let () =
  print_endline "\n=== Dictionary Test ===";
  let dict = D.empty () in
  print_endline "Empty dict created";
  
  let dict = D.insert "name" "Alice" dict in
  print_endline "Inserted name";
  
  let dict = D.insert "age" "25" dict in
  print_endline "Inserted age";
  
  let dict = D.insert "city" "Moscow" dict in
  print_endline "Inserted city";
  
  (match D.find "name" dict with
  | Some v -> Printf.printf "name: %s\n" v
  | None -> print_endline "name not found");
  
  (match D.find "age" dict with
  | Some v -> Printf.printf "age: %s\n" v
  | None -> print_endline "age not found");
  
  (match D.find "city" dict with
  | Some v -> Printf.printf "city: %s\n" v
  | None -> print_endline "city not found");
  
  print_endline "======================\n"
