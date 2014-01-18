let rec last_but_one lst = 
  match lst with
  | [] | [_]-> None
  | [hd; h2] -> Some hd
  | hd::tl -> last_but_one tl;;

(* Example usage and demo *)
assert (last_but_one [1;2;3;4] = Some 3);;
assert (last_but_one [1] = None);;
