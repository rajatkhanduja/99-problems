let rec number_elements lst = 
  match lst with
  | [] -> 0
  | hd::tl -> 1 + (number_elements tl)
;;

assert (number_elements [`a; `b; `c] = 3);;
assert (number_elements [] = 0);;
assert (number_elements [`a] = 1);;
