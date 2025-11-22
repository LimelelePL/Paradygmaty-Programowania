type graph = (int*int list) list
let g = [
  (1,[2;3]);
  (2,[4]);
  (3,[4;5]);
  (4,[]);
  (5,[4])
]

let reachableCount (graph:graph) start = 
  let rec get graph elem = 
    match graph with 
      | head :: tail when fst head = elem -> ( (fst head), (snd head))
      | head :: tail -> get tail elem
      | _ -> failwith "nie znaleziono elementu w grafie!"
  in
  let rec iter accum stack visited = 
    match stack with 
      | [] -> accum
      | top :: bottom -> 
        let (elem, neighbours) = get graph top 
      in
        if List.mem elem visited  then iter accum bottom visited 
        else iter (accum+1) (neighbours @ bottom) (elem :: visited)
      in iter (-1) [start] [];;

      print_int(reachableCount g 5);;


type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let treeStats (tree : int tree) = 
  let rec iter stack maxDepth numOfLeafs = 
    match stack with 
      | [] -> (maxDepth, numOfLeafs)
      | (node, depth) :: tail ->
        match node with 
          | Empty -> 
              iter (tail)(depth)(numOfLeafs)
          | Node(elem, left, right) when left = Empty && right = Empty -> 
              iter (tail)(max depth maxDepth)(numOfLeafs+1)
          | Node(elem, left, right) ->
              iter ((left, depth+1) :: (right, depth+1) :: tail) (max maxDepth depth) numOfLeafs 
      in iter [(tree,1)] 0 0;;

      let tree = Node(1,Node(2,Empty,Empty),Empty);;

      let printTreeStats tree =
        let (depth, num) = treeStats tree in
        Printf.printf "Głębokość: %d, liczba liści: %d\n" depth num;;

      printTreeStats tree;;

      

         