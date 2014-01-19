type 'a rle = 
  | One of 'a
  | Many of int * 'a
;;

let run_length_encoding lst = 
  let rec aux current lst = 
    match lst, current with
    | [], None -> []
    | [], Some t -> t :: []
    | hd::tl,None -> aux (Some (One hd)) tl
    | hd::tl, (Some(One x as t))-> 
        if hd = x then (aux (Some (Many(2, x))) tl) 
        else t :: (aux (Some (One hd)) tl)
    | hd::tl, Some(Many(n, x) as t)  ->
        if hd = x then aux (Some (Many(n + 1, x))) tl
        else t :: (aux (Some (One hd)) tl)
  in
  aux None lst
;;

assert (run_length_encoding [1;1;5;5;5;3] = [Many(2,1); Many(3,5); One 3]);;
