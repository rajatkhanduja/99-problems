let reverse_list lst = 
  let rec rev_list rev = function
    | [] -> rev
    | hd::tl -> (rev_list (hd::rev) tl)
  in
  rev_list [] lst
;;

assert (reverse_list [`a; `b; `c] = [`c; `b; `a]);;
assert (reverse_list [] = []);;
