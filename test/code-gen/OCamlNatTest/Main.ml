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
  | Succ n' -> plus (Succ m) n'

let rec minus m n =
  match m, n with
  | Zero, _ -> Zero
  | _, Zero -> m
  | Succ m', Succ n' -> minus m' n'

let rec mul m n =
  match n with
  | Zero -> Zero
  | Succ n' -> plus (mul m n') m

let n100 = mul ten ten

let n1000 = mul n100 ten

let n10000 = mul n1000 ten

let total = mul n10000 n1000

let rec divmod m n q u =
  match m, u with
  | Zero, _ -> (q, u)
  | Succ m', Zero -> divmod m' n (Succ q) n
  | Succ m', Succ u' -> divmod m' n q u'

let natDiv m n =
  match n with
  | Zero -> Zero
  | Succ n' -> fst (divmod m n' Zero n')

let natMod m n =
  match n with
  | Zero -> Zero
  | Succ n' -> minus n' (snd (divmod m n' Zero n'))

let rec natToStr n =
  match n with
  | (Zero) -> "0"
  | (Succ(Zero)) -> "1"
  | (Succ(Succ(Zero))) -> "2"
  | (Succ(Succ(Succ(Zero)))) -> "3"
  | (Succ(Succ(Succ(Succ(Zero))))) -> "4"
  | (Succ(Succ(Succ(Succ(Succ(Zero)))))) -> "5"
  | (Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))) -> "6"
  | (Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero)))))))) -> "7"
  | (Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))))) -> "8"
  | (Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero)))))))))) -> "9"
  | _ ->
    let x = natDiv n ten in
    let y = natMod n ten in
    natToStr(x) ^ natToStr(y)

let () =
  Printf.printf "%s\n" (natToStr total);
  Printf.printf "%s\n" (natToStr (plus total (Succ Zero)));
