type nat =
  | Zero
  | Succ of nat

let one = Succ Zero

let two = Succ one

let three = Succ two

let four = Succ three

let five = Succ four

let six = Succ five

let seven = Succ six

let eight = Succ seven

let nine = Succ eight

let ten = Succ nine

let rec plus m n =
  match n with
  | Zero -> m
  | Succ n' -> Succ (plus m n')

let rec minus m n =
  match m, n with
  | Zero, _ -> Zero
  | _, Zero -> m
  | Succ m', Succ n' -> minus m' n'

let rec mul m n =
  match n with
  | Zero -> Zero
  | Succ n' -> plus m (mul m n')

let n100 = mul ten ten

let n1000 = mul n100 ten

let n10000 = mul n1000 ten

let n100000 = mul n10000 ten

let n1000000 = mul n100000 ten

let n10000000 = mul n1000000 ten

type 'a mylist = Nil | Cons of 'a * 'a mylist

let rec accum xs a f =
  match xs with
  | Nil -> a
  | Cons (x, xs) -> accum xs (f a x) f

let rev xs = accum xs Nil (fun a x -> Cons (x, a))

let rev' =
  let rec aux a xs = 
    match xs with
    | Nil -> a
    | Cons (x, xs) -> aux (Cons (x, a)) xs
  in
  aux Nil

let rec genlist n =
  match n with
  | Zero -> Cons (Zero, Nil)
  | Succ n -> Cons (Succ n, genlist n)

let listFirstAndLast n =
  match n with
  | Nil -> Zero, Zero
  | Cons (x, Nil) -> x, x
  | Cons (x, xs) ->
      let rec listlast ys =
        match ys with
        | Nil -> Zero
        | Cons (y, Nil) -> y
        | Cons (_, ys) -> listlast ys
      in x, listlast xs

let () =
  let xs = genlist n10000000 in
  let () =
    match listFirstAndLast (rev' (rev' xs)) with
    | (Zero, Zero) -> Printf.printf "0\n"
    | (_, Zero) -> Printf.printf "1\n"
    | (Zero, _) -> Printf.printf "2\n"
    | (_, _) -> Printf.printf "3\n"
  in
    match listFirstAndLast (rev (rev xs)) with
    | (Zero, Zero) -> Printf.printf "0\n"
    | (_, Zero) -> Printf.printf "1\n"
    | (Zero, _) -> Printf.printf "2\n"
    | (_, _) -> Printf.printf "3\n"
