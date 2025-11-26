
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec toLazyList xs =
match xs with
[] -> LNil
| h::t -> LCons(h, function () -> toLazyList t);;

let rec ltake (n, lxs) =
match (n, lxs) with
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;



type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec toLazyList xs =
match xs with
[] -> LNil
| h::t -> LCons(h, function () -> toLazyList t);;

let rec ltake (n, lxs) =
match (n, lxs) with
(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;


let rec ldzialanie xs1 xs2 dzialanie =
  match xs1, xs2 with
  | LNil, LNil -> LNil

  | LNil, LCons (h2, t2) ->
      LCons (h2, fun () -> ldzialanie LNil (t2())  dzialanie)
      
  | LCons (h1, t1), LNil ->
      LCons (h1, fun () -> ldzialanie (t1()) LNil dzialanie)

  | LCons (h1, t1), LCons (h2, t2) ->
      LCons (dzialanie h1 h2,
             fun () -> ldzialanie (t1()) (t2()) dzialanie)



let test = ltake (5 ,ldzialanie (toLazyList [1;2;3]) (toLazyList[2;3;4;5]) (+));;