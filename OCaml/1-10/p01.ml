open Printf

exception Empty_list;;

let rec last_element_of_list l = 
  match l with
  | [] -> raise Empty_list
  | hd::[] -> hd
  | hd::tl -> last_element_of_list tl;;

(* Example usage and demo *)
Printf.printf "%d\n" (last_element_of_list [1;2;3;4]);;
Printf.printf "%d\n" (last_element_of_list [1]);;
Printf.printf "%d\n" (last_element_of_list []);;
