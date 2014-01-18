let rec duplicate_elements = function
  | [] -> []
  | hd::tl -> hd :: hd :: duplicate_elements tl
;;

assert(duplicate_elements [1;3;10] = [1;1;3;3;10;10]);;
assert(duplicate_elements [1;1;3;10] = [1;1;1;1;3;3;10;10]);;
