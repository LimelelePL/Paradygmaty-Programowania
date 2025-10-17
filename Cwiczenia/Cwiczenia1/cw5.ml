let isPalindrome l =
  let rec check (front, back) =
    if back = [] then (true, front)
    else
      let (ok, front') = check (front ,(List.tl back)) in
      if not ok then (false, front')
      else if front' = [] then (true, front')
      else
        let first = List.hd front' in
        let last = List.hd back in
        (first = last, List.tl front')
  in
  fst (check (l, l))


let rec listLength list = 
 if list = [] then 0
 else 1 + listLength (List.tl list);;

let len = listLength([1;2;3]);;
print_int(len)

