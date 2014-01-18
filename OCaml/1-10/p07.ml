type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec join_lists lst1 lst2 =
  match lst1 with
  | hd :: tl -> hd :: join_lists tl lst2
  | [] -> lst2

let rec flatten = function
  | [] -> []
  | One a :: tl -> a :: flatten tl
  | Many a :: tl-> join_lists (flatten a)  (flatten tl)
;;

let example = [ One `a ; Many [ One `b ; Many [ One `c ; One `d ] ; One `e ] ]
;;

assert (flatten example = [ `a ; `b ; `c ; `d ; `e ]);;
