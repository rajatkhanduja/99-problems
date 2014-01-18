let rec remove_at lst k = 
  match lst, k with
  | _::tl, 1 -> tl
  | hd::tl, _ -> hd :: remove_at tl (k-1)
;;


assert (remove_at [10; 9; 12] 1 = [9; 12]);;
assert (remove_at [10; 9; 12] 2 = [10; 12]);;
