
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec toLazyList xs =
match xs with
[] -> LNil
| h::t -> LCons(h, lazy (toLazyList t));;

let rec gen n = 
    LCons(n, lazy (gen (n+1)));;

let rec ltake (n, lxs) =
match (n, lxs) with
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, Lazy.force xf)
;;

let lWybierz xs m n = 
    if n <= 0 then LNil else
    let rec getMthElem xs m = 
        match (m, xs) with 
        (*jezeli m zero lub ujemne to zaczyamy od początku listy*)
            | (m, _) when m <= 0 -> xs 
            | (_, LCons (head, tail)) -> getMthElem (Lazy.force tail) (m-1)
            | (_, LNil) -> LNil
    in 
    let rec skipN xs n = 
        match (n, xs) with
        | (0, LCons(head, tail)) -> xs
        | (_, LCons(head, tail)) -> skipN (Lazy.force tail) (n-1)
        | (_, LNil) -> LNil
    in 
        let rec process xs = 
            match xs with
            | LCons(head, tail) -> LCons (head, lazy (process (skipN (Lazy.force tail) (n-1))))
            | LNil -> LNil
        in process (getMthElem xs (m-1));;

(*test zwykła lista*)
let l1 = toLazyList [1;2;3;4;5;6;7;8];;
let test1 = ltake (1,(lWybierz l1 2 2));;

(*test nieskonczona lista*)
let l2 = gen 4
let test2 = ltake (5,(lWybierz l2 9 9));;

(*test lista stringów*)
let l3 = toLazyList ["a"; "b"; "c"; "d"];;
let test3 = ltake (5,(lWybierz l3 2 2));;

(*test pusta lista*)
let l4 = toLazyList [];;
let test4 = ltake (5,(lWybierz l4 2 2));;

(* test ujemne m*)
let l5 = toLazyList [1;2];;
let test5 = ltake (5,(lWybierz l5 (-3) 10));;

(*test n wieksze niz ilosc elementow w liscie*)
let l6 = toLazyList [1;2];;
let test6 = ltake (5,(lWybierz l6 0 10));;

(*test m wieksze niz ilosc elementów*)
let l7 = toLazyList [1;2];;
let test7 = ltake (5,(lWybierz l7 3 10));;






