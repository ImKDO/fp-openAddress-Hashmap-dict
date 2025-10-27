open Alcotest

module D = Fp_openAddress_Hashmap_dict.Dict.OaDict

let test_empty () =
  let dict = D.empty () in
  check (option string) "empty dict find returns None" None (D.find "key" dict)

let test_insert_find () =
  let dict = D.empty () in
  let dict = D.insert "name" "Alice" dict in
  check (option string) "find inserted key" (Some "Alice") (D.find "name" dict);
  check (option string) "find non-existent key" None (D.find "age" dict)

let test_insert_multiple () =
  let dict = D.empty () in
  let dict = D.insert "a" "1" dict in
  let dict = D.insert "b" "2" dict in
  let dict = D.insert "c" "3" dict in
  check (option string) "find 'a'" (Some "1") (D.find "a" dict);
  check (option string) "find 'b'" (Some "2") (D.find "b" dict);
  check (option string) "find 'c'" (Some "3") (D.find "c" dict)

let test_insert_update () =
  let dict = D.empty () in
  let dict = D.insert "key" "old" dict in
  let dict = D.insert "key" "new" dict in
  check (option string) "update overwrites old value" (Some "new") (D.find "key" dict)

(* Тесты для remove *)
let test_remove () =
  let dict = D.empty () in
  let dict = D.insert "a" "1" dict in
  let dict = D.insert "b" "2" dict in
  let dict = D.remove "a" dict in
  check (option string) "removed key not found" None (D.find "a" dict);
  check (option string) "other key still exists" (Some "2") (D.find "b" dict)

let test_remove_non_existent () =
  let dict = D.empty () in
  let dict = D.insert "a" "1" dict in
  let dict = D.remove "b" dict in
  check (option string) "removing non-existent key doesn't affect dict" (Some "1") (D.find "a" dict)

let test_filter () =
  let dict = D.empty () in
  let dict = D.insert "a" "1" dict in
  let dict = D.insert "b" "2" dict in
  let dict = D.insert "c" "3" dict in
  let filtered = D.filter (fun _k v -> int_of_string v > 1) dict in
  check (option string) "filtered out 'a'" None (D.find "a" filtered);
  check (option string) "kept 'b'" (Some "2") (D.find "b" filtered);
  check (option string) "kept 'c'" (Some "3") (D.find "c" filtered)

let test_fold_left () =
  let dict = D.empty () in
  let dict = D.insert "a" "1" dict in
  let dict = D.insert "b" "2" dict in
  let dict = D.insert "c" "3" dict in
  let sum = D.fold_left (fun acc _k v -> acc + int_of_string v) 0 dict in
  check int "sum of values" 6 sum

let test_fold_left_empty () =
  let dict = D.empty () in
  let sum = D.fold_left (fun acc _k v -> acc + int_of_string v) 0 dict in
  check int "fold_left on empty dict" 0 sum

let test_fold_right () =
  let dict = D.empty () in
  let dict = D.insert "a" "1" dict in
  let dict = D.insert "b" "2" dict in
  let dict = D.insert "c" "3" dict in
  let sum = D.fold_right (fun _k v acc -> acc + int_of_string v) dict 0 in
  check int "sum of values (fold_right)" 6 sum

let test_equal_same () =
  let d1 = D.empty () in
  let d1 = D.insert "a" "1" d1 in
  let d1 = D.insert "b" "2" d1 in
  
  let d2 = D.empty () in
  let d2 = D.insert "a" "1" d2 in
  let d2 = D.insert "b" "2" d2 in
  
  check bool "equal dicts" true (D.equal (=) d1 d2)

let test_equal_different () =
  let d1 = D.empty () in
  let d1 = D.insert "a" "1" d1 in
  
  let d2 = D.empty () in
  let d2 = D.insert "a" "2" d2 in
  
  check bool "different values" false (D.equal (=) d1 d2)

let test_equal_different_size () =
  let d1 = D.empty () in
  let d1 = D.insert "a" "1" d1 in
  let d1 = D.insert "b" "2" d1 in
  
  let d2 = D.empty () in
  let d2 = D.insert "a" "1" d2 in
  
  check bool "different sizes" false (D.equal (=) d1 d2)

let test_append_dict () =
  let d1 = D.empty () in
  let d1 = D.insert "a" "1" d1 in
  let d1 = D.insert "b" "2" d1 in
  
  let d2 = D.empty () in
  let d2 = D.insert "c" "3" d2 in
  let d2 = D.insert "d" "4" d2 in
  
  let merged = D.append_dict d1 d2 in
  check (option string) "merged has 'a'" (Some "1") (D.find "a" merged);
  check (option string) "merged has 'b'" (Some "2") (D.find "b" merged);
  check (option string) "merged has 'c'" (Some "3") (D.find "c" merged);
  check (option string) "merged has 'd'" (Some "4") (D.find "d" merged)

let test_append_dict_overlap () =
  let d1 = D.empty () in
  let d1 = D.insert "a" "1" d1 in
  let d1 = D.insert "b" "2" d1 in
  
  let d2 = D.empty () in
  let d2 = D.insert "a" "10" d2 in
  let d2 = D.insert "c" "3" d2 in
  
  let merged = D.append_dict d1 d2 in
  check (option string) "d1 overwrites d2 for 'a'" (Some "1") (D.find "a" merged);
  check (option string) "merged has 'b'" (Some "2") (D.find "b" merged);
  check (option string) "merged has 'c'" (Some "3") (D.find "c" merged)

let test_large_dict () =
  let dict = ref (D.empty ()) in
  for i = 0 to 999 do
    dict := D.insert (string_of_int i) (string_of_int (i * 2)) !dict
  done;
  check (option string) "large dict find 0" (Some "0") (D.find "0" !dict);
  check (option string) "large dict find 500" (Some "1000") (D.find "500" !dict);
  check (option string) "large dict find 999" (Some "1998") (D.find "999" !dict)

(* Набор тестов *)
let basic_tests = [
  "empty dict", `Quick, test_empty;
  "insert and find", `Quick, test_insert_find;
  "insert multiple", `Quick, test_insert_multiple;
  "insert update", `Quick, test_insert_update;
]

let remove_tests = [
  "remove", `Quick, test_remove;
  "remove non-existent", `Quick, test_remove_non_existent;
]

let filter_tests = [
  "filter", `Quick, test_filter;
]

let fold_tests = [
  "fold_left", `Quick, test_fold_left;
  "fold_left empty", `Quick, test_fold_left_empty;
  "fold_right", `Quick, test_fold_right;
]

let equal_tests = [
  "equal same", `Quick, test_equal_same;
  "equal different values", `Quick, test_equal_different;
  "equal different sizes", `Quick, test_equal_different_size;
]

let append_tests = [
  "append_dict", `Quick, test_append_dict;
  "append_dict overlap", `Quick, test_append_dict_overlap;
]

let stress_tests = [
  "large dict (1000 elements)", `Slow, test_large_dict;
]

let () =
  run "OaDict" [
    "Basic operations", basic_tests;
    "Remove operations", remove_tests;
    "Filter operations", filter_tests;
    "Fold operations", fold_tests;
    "Equality operations", equal_tests;
    "Append operations", append_tests;
    "Stress tests", stress_tests;
  ]
