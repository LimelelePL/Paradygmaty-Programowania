

let rec flatten list = 
  if list = [] then []
  else if List.tl list = [] then List.hd list
  else List.hd list @ flatten(List.tl list);;
  
  let flattened = flatten [[3; 4]; [3; 6]];;

