type PairSeq = [(Int,Int)]

(!!!)::PairSeq->(Int,Int)->Int
xs !!! (x,y) = case y of
    0 -> a
    1 -> b
    where
        (a,b) = xs !! x

expand::PairSeq->Int->PairSeq
expand [] _ = []
expand xs n = reverse $ expand' (reverse xs) n

expand'::PairSeq->Int->PairSeq
expand' ((0,0):xs) _ = xs
expand' (x:xs) n = (vann badPart d n) ++ xs
    where
        badPart  = take badRoot xs
        d = if snd x == 0 
            then 0 
            else fst x - xs !!! (badRoot - 1,0)
        badRoot = root (x:xs) 0

vann::PairSeq->Int->Int->PairSeq
vann _ _ 0 = []
vann b d n = (vann b' d (n-1)) ++ b'
    where
        b' = map (\(x,y)->(x+d,y)) b

root::PairSeq->Int->Int
root xs x = if snd (xs !! 0) == 0
                then root0 xs x
                else root1 xs x
     
root0::PairSeq->Int->Int
root0 xs x = last strongPart
    where
        strongPart = fst $ break (\k->fml k < fml (head j)) j
        j = [k | k<-[1..(length xs - 1)] ,p (p x) == p k]
        fml z =  (xs !! z):(fst $ break (< piv z) (reverse (take z xs)))
        piv z = (1 + xs !!! (z,0), xs !!! (x,1) )
        p = parent xs 


parent::PairSeq->(Int->Int)
parent xs = \z -> if xs !!! (z,0) == 0
    then -1
    else minimum [k | k<-[z..(length xs - 1)] , xs !!! (k,0) < xs !!! (z,0) ]

root1::PairSeq->Int->Int
root1 xs x = soe !! (root s' 0)
    where
        s' = zip (map (\k->xs !!! (k,1)) soe) (take (xs00 + 1) (repeat 0))
        soe = take (xs00 + 1) (iterate rooxs 0)
        rooxs = root0 xs
        xs00 = xs !!! (0,0)

calc::PairSeq->Int->Int
calc [] n = n
calc xs n = calc (expand xs n) (10^n)

hamPair::Int->Int
hamPair n = calc xs n
    where
        xs = zip [0..n] [0..n]

gosei::Int->Int
gosei 1 = hamPair 86
gosei n = hamPair (gosei (n-1))

main :: IO ()
main = print $ gousei 86