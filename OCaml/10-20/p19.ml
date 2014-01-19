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

let rotate list n = 
  let len = List.length list 
  in 
    (* Compute a rotation value between 0 and len-1 *)
    let n = if len = 0 then 0 else (n mod len + len) mod len 
    in
      if n = 0 then list
      else let a, b = split list n 
      in b @ a
;;


assert (rotate [1;2;3;4;5;6] 3 = [4;5;6;1;2;3]);;
assert (rotate [1;2;3;4;5;6] 4 = [5;6;1;2;3;4]);;
