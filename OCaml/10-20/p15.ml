let rec join_list lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | hd::tl -> hd :: (join_list tl lst2)

let rec replicate_elements lst n = 
  let rec repeat_elem e = function
    | 0 -> []
    | _ as n-> e :: repeat_elem e (n - 1)
  in
  match lst with
  | [] -> []
  | hd :: tl -> join_list (repeat_elem hd n) (replicate_elements tl n)
;;


assert (replicate_elements [`a;`b;`c] 3 = [`a;`a;`a;`b;`b;`b;`c;`c;`c]);;
