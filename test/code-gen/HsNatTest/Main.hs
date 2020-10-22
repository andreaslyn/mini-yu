module Main (main, plus) where

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
plus m (Succ n) = plus (Succ m) n

minus :: Nat -> Nat -> Nat
minus Zero _ = Zero
minus m Zero = m
minus (Succ m) (Succ n) = minus m n

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (Succ n) = plus (mul m n) m

n100 :: Nat
n100 = mul ten ten

n1000 :: Nat
n1000 = mul n100 ten

n10000 :: Nat
n10000 = mul n1000 ten

total :: Nat
total = mul n10000 n1000

divmod :: Nat -> Nat -> Nat -> Nat -> (Nat, Nat)
divmod Zero  _ q u = (q, u)
divmod (Succ m) n q Zero = divmod m n (Succ q) n
divmod (Succ m) n q (Succ u) = divmod m n q u

natDiv :: Nat -> Nat -> Nat
natDiv _ Zero = Zero
natDiv m (Succ n) = fst (divmod m n Zero n)

natMod :: Nat -> Nat -> Nat
natMod _ Zero = Zero
natMod m (Succ n) = minus n (snd (divmod m n Zero n))

natToStr :: Nat -> String
natToStr (Zero) = "0"
natToStr (Succ(Zero)) = "1"
natToStr (Succ(Succ(Zero))) = "2"
natToStr (Succ(Succ(Succ(Zero)))) = "3"
natToStr (Succ(Succ(Succ(Succ(Zero))))) = "4"
natToStr (Succ(Succ(Succ(Succ(Succ(Zero)))))) = "5"
natToStr (Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))) = "6"
natToStr (Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero)))))))) = "7"
natToStr (Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))))) = "8"
natToStr (Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero)))))))))) = "9"
natToStr (n) =
  let x = natDiv n ten
      y = natMod n ten
  in natToStr(x) ++ natToStr(y)

main :: IO ()
main = do
  putStrLn (natToStr total)
  putStrLn (natToStr (plus total (Succ Zero)))
