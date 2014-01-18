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

assert (run_length_encoding [1;1;2;3;3] = [(2,1); (1,2); (2,3)]);;
