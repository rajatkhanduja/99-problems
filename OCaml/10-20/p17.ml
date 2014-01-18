let split lst l = 
  let rec split_p1 lst l = 
    match lst, l with
    | _ :: tl, 0 -> []
    | hd :: tl, _ -> hd :: split_p1 tl (l - 1)
  and split_p2 lst l = 
    match lst, l with 
    | hd :: tl, 0 -> hd::tl
    | _ :: tl, _ -> split_p2 tl (l - 1)
  in
  (split_p1 lst l, split_p2 lst l)
;;

assert (split [1;2;3;4;5;6] 2 = ([1;2], [3;4;5;6]));;
