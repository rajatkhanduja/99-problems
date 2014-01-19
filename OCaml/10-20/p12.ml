type 'a rle = 
  | One of 'a
  | Many of int * 'a
;;

let rec rle_to_list = function
  | [] -> []
  | One(x) :: tl -> x :: (rle_to_list tl)
  | Many(2, x) :: tl -> x :: (rle_to_list (One(x) :: tl))
  | Many(n, x) :: tl -> x :: (rle_to_list (Many(n - 1, x) :: tl))
;;

assert(rle_to_list [One 1; Many (2,3); Many (3,10);] = [1; 3; 3; 10; 10 ;10]);;
