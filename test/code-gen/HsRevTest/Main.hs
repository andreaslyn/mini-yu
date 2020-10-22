module Main (main) where

data Nat = Zero | Succ Nat

one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

three :: Nat
three = Succ two

four :: Nat
four = Succ three 
five :: Nat
five = Succ four

six :: Nat
six = Succ five

seven :: Nat
seven = Succ six

eight :: Nat
eight = Succ seven

nine :: Nat
nine = Succ eight

ten :: Nat
ten = Succ nine

plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ n) = Succ (plus m n)

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (Succ n) = plus m (mul m n)

n100 :: Nat
n100 = mul ten ten

n1000 :: Nat
n1000 = mul n100 ten

n10000 :: Nat
n10000 = mul n1000 ten

n100000 :: Nat
n100000 = mul n10000 ten

n1000000 :: Nat
n1000000 = mul n100000 ten

n10000000 :: Nat
n10000000 = mul n1000000 ten

data List a = Nil | Cons a (List a)

accum :: List(b) -> a -> (a -> b -> a) -> a
accum Nil a _ = a
accum (Cons x xs) a f = accum xs (f a x) f

rev :: List a -> List a
rev xs = accum xs Nil (\a x -> Cons x a)

rev' :: List(a) -> List(a)
rev' = aux Nil
  where
    aux :: List(a) -> List(a) -> List(a)
    aux a Nil = a
    aux a (Cons x xs) = aux (Cons x a) xs

genlist :: Nat -> List(Nat)
genlist Zero = Cons Zero Nil
genlist (Succ n) = Cons (Succ n) (genlist n)

listFirstAndLast :: List Nat -> (Nat, Nat)
listFirstAndLast Nil = (Zero, Zero)
listFirstAndLast (Cons x Nil) = (x, x)
listFirstAndLast (Cons x xs) = (x, listlast xs)
  where
    listlast :: List Nat -> Nat
    listlast Nil = Zero
    listlast (Cons x Nil) = x
    listlast (Cons _ xs) = listlast xs

main :: IO ()
main = do
  let xs = genlist n10000000
  case listFirstAndLast (rev' (rev' xs)) of
    (Zero, Zero) -> putStrLn "0"
    (_, Zero) -> putStrLn "1"
    (Zero, _) -> putStrLn "2"
    (_, _) -> putStrLn "3"
  case listFirstAndLast (rev (rev xs)) of
    (Zero, Zero) -> putStrLn "0"
    (_, Zero) -> putStrLn "1"
    (Zero, _) -> putStrLn "2"
    (_, _) -> putStrLn "3"
