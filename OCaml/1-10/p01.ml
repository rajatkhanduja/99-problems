let rec last_element_of_list l = 
  match l with
  | [] -> None
  | hd::[] -> Some hd
  | hd::tl -> last_element_of_list tl;;

assert (last_element_of_list [`a;`b; `c; `d] = Some `d);;
assert (last_element_of_list [1] = Some 1);;
assert (last_element_of_list [] = None);;
