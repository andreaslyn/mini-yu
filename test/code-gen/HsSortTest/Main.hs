{-# LANGUAGE BangPatterns #-}


partition :: [a] -> (a -> Bool) -> ([a], [a])
partition [] _ = ([], [])
partition (x : xs) f =
  let (a, b) = partition xs f in
  case f x of
    True -> (x : a, b)
    False -> (a, x : b)


qsort :: [a] -> (a -> a -> Bool) -> [a]
qsort [] _ = []
qsort (x : xs) f =
  let (!ls, !rs) = partition xs (\a -> f a x)
      !ls' = qsort ls f
      !rs' = qsort rs f
  in ls' ++ x : rs'


msort :: [a] -> (a -> a -> Bool) -> [a]
msort origList cmp = mergeAll (sequences origList)
  where
    sequences (a:b:xs)
      | a `cmp` b == False = descending b [a]  xs
      | otherwise          = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == False = descending b (a:as) bs
    descending a as bs     = (a:as) : sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= False = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs      = let !x = as [a]
                             in x : sequences bs

    mergeAll (x : []) = x
    mergeAll xs       = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == False = b:merge as  bs'
      | otherwise          = a:merge as' bs
    merge [] bs            = bs
    merge as []            = as


data Nat = Z | S Nat


genlist :: Nat -> [Bool]
genlist Z = []
genlist (S n) = True : False : genlist n


plus :: Nat -> Nat -> Nat
plus m Z = m
plus m (S n) = plus (S m) n


mul :: Nat -> Nat -> Nat
mul _ Z = Z
mul m (S n) = plus (mul m n) m


n10 :: Nat
n10 = S (S (S (S (S (S (S (S (S (S Z)))))))))

n100 :: Nat
n100 = mul n10 n10

n1'000 :: Nat
n1'000 = mul n100 n10

mtotal :: Nat
mtotal = mul n1'000 n100

qtotal :: Nat
qtotal = mul n1'000 (S (S (S (S Z))))


isOrdered :: (Bool -> Bool -> Bool) -> [Bool] -> Bool
isOrdered _ [] = True
isOrdered _ (_ : []) = True
isOrdered cmp (x1 : x2 : xs) =
  case cmp x1 x2 of
    False -> False
    True -> isOrdered cmp (x2 : xs)


main :: IO ()
main = do
  let ms = genlist mtotal
  case isOrdered (<=) (msort ms (<=)) of
    True -> putStrLn "1"
    False -> putStrLn "0"
  case isOrdered (>=) (msort ms (>=)) of
    True -> putStrLn "1"
    False -> putStrLn "0"
  let qs = genlist qtotal
  case isOrdered (<=) (qsort qs (<=)) of
    True -> putStrLn "1"
    False -> putStrLn "0"
  case isOrdered (>=) (qsort qs (>=)) of
    True -> putStrLn "1"
    False -> putStrLn "0"
