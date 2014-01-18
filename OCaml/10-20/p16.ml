let drop_every_nth lst n =
  let rec drop_aux lst k n =
    match lst,k with 
    | [], _ -> []
    | hd :: tl, 1 -> drop_aux tl n n
    | hd :: tl, _ -> hd :: drop_aux tl (k - 1) n
  in
  drop_aux lst n n
;;

assert (drop_every_nth [`a; `b; `c; `d] 2 = [`a; `c]);;
assert (drop_every_nth [`a; `b; `c; `d] 1 = []);;
assert (drop_every_nth [`a; `b; `c; `d] 3 = [`a; `b; `d]);;
