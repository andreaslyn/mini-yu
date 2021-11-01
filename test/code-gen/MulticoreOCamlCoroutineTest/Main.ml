
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

type ('x, 'y, 'a) lazylist = LNil of 'a | LCons of 'y * ('x -> ('x, 'y, 'a) lazylist)

effect CoYield : nat -> unit

let rec coRoutineSum n =
  match n with
  | Zero -> ()
  | Succ n ->
      perform (CoYield n);
      perform (CoYield n);
      perform (CoYield n);
      coRoutineSum n

let rec lastLazyList l =
  match l with
  | LNil _ -> ten
  | LCons (x, xs) ->
      match xs () with
      | LNil _ -> x
      | l' -> lastLazyList l'

let runCoRoutine action =
  try
    action ();
    LNil ()
  with effect (CoYield n) k ->
    LCons (n, fun () -> continue k ())


let runCoRoutineSum n = lastLazyList (runCoRoutine (fun () -> coRoutineSum n))

let total = mul (mul (mul (mul (mul ten ten) ten) ten) ten) ten

let () =
  Printf.printf "%s\n" (natToStr (runCoRoutineSum total));
  Printf.printf "%s\n" (natToStr (runCoRoutineSum (Succ total)));
  Printf.printf "%s\n" (natToStr (runCoRoutineSum (Succ (Succ total))))
