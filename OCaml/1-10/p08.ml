let eliminate_consecutive_dupl lst = 
  let rec remove_duplicates lst prev_elem =
    match lst, prev_elem with
    | [], _ -> []
    | hd :: tl, (Some elem) when elem = hd -> remove_duplicates tl (Some hd)
    | hd :: tl, _ -> hd :: remove_duplicates tl (Some hd)
  in
  remove_duplicates lst None
;;

assert(eliminate_consecutive_dupl [1;2;2] = [1;2]);;
assert(eliminate_consecutive_dupl [1;2] = [1;2]);;
assert(eliminate_consecutive_dupl [1;1;2] = [1;2]);;
assert(eliminate_consecutive_dupl [1;2;2;3;3;3] = [1;2;3]);;
