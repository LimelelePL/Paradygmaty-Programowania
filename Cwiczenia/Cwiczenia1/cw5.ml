let isPalindrome list = 
  list = List.rev list (*nie bylo na wykladzie*)


let listLength list = 
  let rec iter (l, acc) =
  match l with
  | [] -> acc
  |  _  -> iter(List.tl l, acc+1)
in iter(list, 0);;

let len = listLength([1;2;3]);;
print_int(len)

