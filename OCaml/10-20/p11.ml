type 'a rle = 
  | One of 'a
  | Many of int * 'a
;;

let pack_consecutive lst = 
  let rec pack_aux acc current = function
    | [] -> []
    | [x] -> (x::current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then pack_aux acc (a :: current) t
        else pack_aux ((a :: current)::acc) [] t
  in
  List.rev (pack_aux [] [] lst)
;;

let run_length_encoding lst = 
  let rec conv acc = function
    | [] -> acc
    | (a ::_ as t) :: tl -> conv ((List.length t, a) :: acc) tl
  in
  List.rev (conv [] (pack_consecutive lst))
;;

let rec modified_lst_from_nested_lst lst =
  match lst with
  | [] -> []
  | (1, c) :: tl -> One (c) :: (modified_lst_from_nested_lst tl)
  | (n, c) :: tl -> Many((n, c)) :: (modified_lst_from_nested_lst tl) 
;;

let modified_rle lst = 
  modified_lst_from_nested_lst (run_length_encoding lst)
;;

assert (modified_rle [1;1;2;3;3] = [Many((2,1)); One(2); Many((2,3))]);;
