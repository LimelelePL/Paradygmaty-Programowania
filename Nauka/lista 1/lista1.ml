
let fiddle4 (a,b,c,d) = (d,b,c,a);;

let rec hits (list1, list2) = 
  if list1 = [] || list2 = [] then 0
  else if List.hd list1 = List.hd list2 then (1+hits(List.tl list1, List.tl list2))
  else hits(List.tl list1, List.tl list2);;

let test = hits ([1;2;3], [2;2;3]);;

let rec insert (list, elem, pos) = 
  if list = [] then elem::list
  else if pos > 0 then List.hd list ::insert(List.tl list, elem, pos-1)
  else elem :: list ;;

let inserted = insert([1;2;3], 10, 1);;

let militaryMinutes (hour, minute, str) =
  if hour > 12 || hour < 0 then failwith"wrong time"
 else
    let convertHour = 
    if(hour + 12 >= 24 ) then (hour - 12)
    else (hour + 12)
  in
    if str = "PM" then string_of_int convertHour ^ ": " ^ string_of_int(minute)
    else if str = "AM" then string_of_int hour^ ": " ^ string_of_int(minute)
    else failwith("wrong str") ;;

let te = militaryMinutes(12, 30, "PM") ;;

