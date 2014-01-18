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

assert (pack_consecutive [1;2;2;2;3;3;4] = [[1]; [2;2;2]; [3;3]; [4]]);;
