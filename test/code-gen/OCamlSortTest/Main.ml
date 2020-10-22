
let rec partition xs0 f =
  match xs0 with
  | [] -> [], []
  | x :: xs ->
    let a, b = partition xs f in
    match f x with
    | true -> (x :: a, b)
    | false -> (a, x :: b)


let rec qsort xs0 f =
  match xs0 with
  | [] -> []
  | x :: xs ->
      let ls, rs = partition xs (fun a -> f a x)
      in qsort ls f @ x :: qsort rs f

let msort origList cmp =
  let rec sequences xs0 =
    match xs0 with
    | a :: b :: xs ->
      begin
        match cmp a b with
        | false -> descending b [a]  xs
        | true -> ascending  b (fun t -> a :: t) xs
      end
    | _ -> [xs0]
  and descending a0 as0 bs0 =
    match bs0 with
    | b :: bs ->
      begin
        match cmp a0 b with
        | false -> descending b (a0 :: as0) bs
        | true -> (a0 :: as0) :: sequences bs0
      end
    | _ -> (a0 :: as0) :: sequences bs0
  and ascending a0 as0 bs0 =
    match bs0 with
    | b :: bs ->
      begin
        match cmp a0 b with
        | true -> ascending b (fun ys -> as0 (a0 :: ys)) bs
        | false -> as0 [a0] :: sequences bs0
      end
    | _ -> as0 [a0] :: sequences bs0
  and mergeAll xs0 =
    match xs0 with
    | [x] -> x
    | xs -> mergeAll (mergePairs xs)
  and mergePairs xs0 =
    match xs0 with
    | a :: b :: xs -> merge a b :: mergePairs xs
    | xs -> xs
  and merge as0 bs0 =
    match as0, bs0 with
    | a :: as', b :: bs' ->
      begin
        match cmp a b with
        | false -> b :: merge as0 bs'
        | true -> a :: merge as' bs0
      end
    | [], bs' -> bs'
    | as', [] -> as'
  in mergeAll (sequences origList)


type nat = Z | S of nat


let rec genlist n0 =
  match n0 with
  | Z -> []
  | S n -> true :: false :: genlist n


let rec plus m n0 =
  match n0 with
  | Z -> m
  | S n -> plus (S m) n


let rec mul m n0 =
  match n0 with
  | Z -> Z
  | S n -> plus (mul m n) m


let n10 = S (S (S (S (S (S (S (S (S (S Z)))))))))

let n100 = mul n10 n10

let n1'000 = mul n100 n10

let mtotal = mul n1'000 n100

let qtotal = mul n1'000 (S (S (S (S Z))))


let rec isOrdered cmp xs0 =
  match xs0 with
  | [] -> true
  | [_] -> true
  | x1 :: x2 :: xs ->
      match cmp x1 x2 with
      | false -> false
      | true -> isOrdered cmp (x2 :: xs)


let ale b1 b2 =
  match b1, b2 with
  | true, false -> false
  | _, _ -> true

let age b1 b2 =
  match b1, b2 with
  | false, true -> false
  | _, _ -> true


let () =
  let ms = genlist mtotal in
  let () =
    match isOrdered ale (msort ms ale) with
    | true -> Printf.printf "1\n"
    | false -> Printf.printf "0\n" in
  let () =
    match isOrdered age (msort ms age) with
    | true -> Printf.printf "1\n"
    | false -> Printf.printf "0\n" in
  let qs = genlist qtotal in
  let () =
    match isOrdered ale (qsort qs ale) with
    | true -> Printf.printf "1\n"
    | false -> Printf.printf "0\n"
  in
    match isOrdered age (qsort qs age) with
    | true -> Printf.printf "1\n"
    | false -> Printf.printf "0\n"
