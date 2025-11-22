type eval = Num of float | Add | Sub | Mul | Div;;

let evaluate list = 
    let pop stack =
        match stack with
        | head :: tail -> (head, tail)
        | [] -> failwith "pusty stos!"
    in 
    let push stack elem = elem :: stack
in let rec helper list stack = 
    match list with
    | head :: tail ->
        (
        match head with
            | Num number -> helper tail (push stack number)
            | Add -> let (num1, st1) = pop stack in let (num2, st2) = pop st1
         in helper tail (push st2 (num2 +. num1))
            | Sub -> let (num1, st1) = pop stack in let (num2, st2) = pop st1
         in helper tail (push st2 (num2 -. num1))
            | Mul -> let (num1, st1) = pop stack in let (num2, st2) = pop st1
         in helper tail (push st2 (num2 *. num1))
            | Div -> let (num1, st1) = pop stack in let (num2, st2) = pop st1
         in helper tail (push st2 (num2 /. num1))
        )
        (*jezeli nasza lista bedzie pusta oznacza to ze wszystkie operacje zostaly przetworzone i
                    i jedynym elementem na stosie powinien być wynik wszystkich operacji *)
        | [] -> 
            (
            match stack with
            | head :: [] -> head
            | _ -> failwith "nieprawidlowa ilosc argumentow"
            )

    in helper list [];;
