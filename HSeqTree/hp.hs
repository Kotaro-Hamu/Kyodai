expand::[(Int,Int)]->Int->[(Int,Int)]
expand [] _ = []
expand xs n = reverse $ expand' (reverse xs) n

expand'::[(Int,Int)]->Int->[(Int,Int)]
expand' ((0,0):xs) _ = xs
expand' (x:xs) n = (vann b d n) ++ xs
    where
        b  = take r xs
        d = if snd x == 0 then 0 else fst x - fst (xs !! (r - 1))
        r = root (x:xs) 0

vann::[(Int,Int)]->Int->Int->[(Int,Int)]
vann _ _ 0 = []
vann b d n = (vann b' d (n-1)) ++ b'
    where
        b' = map (\(x,y)->(x+d,y)) b

root::[(Int,Int)]->Int->Int
root ts x = if snd (ts !! 0) == 0
                then root0 ts x
                else root1 ts x
     
root0::[(Int,Int)]->Int->Int
root0 ts x = last sj
    where
        (sj,_) = break (\k->fml k < fml (head j)) j
        j = [k | k<-[1..len] ,p (p x) == p k]
        fml z =  (ts !! z):(reverse $ fst $ break (< piv z) (reverse (take z ts)))
        piv z = (fst(ts !! z)+1,snd(ts !! x))
        p z = if fst (ts !! z) == 0 
                then -1 
                else minimum [k | k<-[z..len] , fst (ts !! k) < fst (ts !! z) ]
        len = length ts - 1

root1::[(Int,Int)]->Int->Int
root1 ts x = soe !! (root s' 0)
    where
        s' = zip (map (\k->snd (ts !! k)) soe) (take (ts00 + 1) (repeat 0))
        soe = take (ts00 + 1) (iterate roots 0)
        roots = root0 ts
        ts00 = fst(ts !! 0)

main :: IO ()
main = print $ expand [(0,0),(1,1),(1,0),(1,0),(2,1)] 3

--展開しつつ角括弧の中を増やす感じ
keisan::[(Int,Int)]->Int->Int
keisan [] n = n
keisan s n = keisan (expand s n) 10^n

--ハムペアの限界を召喚する
hamPair::Int->Int 
hamPair n = expand s n
    where s = zip [0..n] [0..n]

--fをたくさん自己合成した関数を返す
gousei :: (Int -> Int) -> Int -> (Int -> Int)
gousei f 1 = f
gousei f n = f . gousei f (n-1)

main::IO()
main = print $ gousei hamPair 86 86
