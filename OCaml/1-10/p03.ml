let rec kth_element lst k = 
  match lst, k with
  | [], _ -> None
  | hd::tl, 1 -> Some hd
  | hd::tl, _ -> kth_element tl (k - 1)
;;

assert (kth_element [`a;`b;`c;`d] 3 = Some `c);;
assert (kth_element [`a;`b;`c;`d] 5 = None);;
assert (kth_element [`a;`b;`c;`d] 4 = Some `d);;
assert (kth_element [`a;`b;`c;`d] 1 = Some `a);;
