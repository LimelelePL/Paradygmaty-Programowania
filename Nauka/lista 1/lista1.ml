
let fiddle4 (a,b,c,d) = (d,b,c,a);;

let rec hits (list1, list2) = 
  if list1 = [] || list2 = [] then 0
  else if List.hd list1 = List.hd list2 then (1+hits(List.tl list1, List.tl list2))
  else hits(List.tl list1, List.tl list2);;

let test = hits ([1;2;3], [2;2;3]);;