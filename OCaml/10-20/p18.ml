let rec slice lst start_pos end_pos = 
  match lst, start_pos, end_pos with
  | _, 1, 0 -> []
  | hd::tl, 1, _ -> hd::(slice tl 1 (end_pos - 1))
  | hd::tl, _, _ -> slice tl (start_pos - 1) (end_pos - 1)
;;

assert (slice [1;2;3;4;5;6] 2 4 = [2;3;4]);;
assert (slice [1;2;3;4;5;6] 2 6 = [2;3;4;5;6]);;
assert (slice [1;2;3;4;5;6] 1 6 = [1;2;3;4;5;6]);;
