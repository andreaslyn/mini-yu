{-# LANGUAGE RankNTypes #-}

import Control.Monad.Trans.Cont


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



data LazyList x y a = LNil a | LCons y (x -> LazyList x y a)


{-# INLINE coyield #-}
coyield :: forall x y a. y -> Cont (LazyList x y a) x
coyield y = cont (LCons y)


{-# INLINE runCoRoutine #-}
runCoRoutine :: forall x y a. Cont (LazyList x y a) a -> LazyList x y a
runCoRoutine m = runCont m LNil


coRoutineSum ::
  Nat -> Cont (LazyList () Nat ()) ()
coRoutineSum Zero = return ()
coRoutineSum (Succ n) = do
  coyield n
  coyield n
  coyield n
  coRoutineSum n


lastLazyList :: LazyList () Nat () -> Nat
lastLazyList (LNil ()) = ten
lastLazyList (LCons x xs) =
  case xs () of
    LNil () -> x
    xs' -> lastLazyList xs'


runCoRoutineSum :: Nat -> Nat
runCoRoutineSum n = lastLazyList (runCoRoutine (coRoutineSum n))


total = mul (mul (mul (mul (mul ten ten) ten) ten) ten) ten


main :: IO ()
main = do
  putStrLn (natToStr $ runCoRoutineSum total)
  putStrLn (natToStr $ runCoRoutineSum (Succ total))
