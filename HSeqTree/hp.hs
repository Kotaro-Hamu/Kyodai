--ペア数列と自然数から展開した自然数を出力する
tenkai::[(Int,Int)]->Int->[(Int,Int)]
tenkai [] _ = []
tenkai s n = reverse $ tenkai' (reverse s) n

--プログラムでは右から左に数列を並べたほうが扱いやすいので反転
--刈子の添字で挙動を変える
tenkai'::[(Int,Int)]->Int->[(Int,Int)]
tenkai' ((0,0):s) _ = s
tenkai' ((x,y):s) n = if y == 0
    then yokoVan ((x,y):s) n
    else tateVan ((x,y):s) n

--刈子の添字が0なら自分より強い家族ごと横に増やす
yokoVan::[(Int,Int)]->Int->[(Int,Int)]
yokoVan ((x,y):s) n = van b 0 n ++ s
    where
        b = take r s
        r = root0 ((x,y):s) 0

--ハムペアにとって最も原始的な親探査
--s1xによって家族の定義が変わる
root0::[(Int,Int)]->Int->Int
root0 s x = if s1x == 0
        then fb0 s x
        else fb1 s x
    where s1x = snd $ s !! x

--s1xが0のときの家族バトル
--家族の定義はfml0
fb0::[(Int,Int)]->Int->Int
fb0 s x = if null oji ||fml oya == z 
    then last (oya:oji)
    else minimum [t | t <- oya:oji, fml t == z]
    where 
        z = minimum [fml t | t <- oji]
        (oya:oji) = [t | t <- [x..length s -1] , p (p x) == p t]
        fml = fml0 s
        p = p0 s

--s1xが0でないときの家族バトル
--家族の定義はfml1
fb1::[(Int,Int)]->Int->Int
fb1 s x = if null oji ||fml oya == z
    then last (oya:oji)
    else minimum [t | t <- oya:oji, fml t == z]
    where 
        z = minimum [fml t | t <- oji]
        (oya:oji) = [t | t <- [x..length s -1] , p (p x) == p t]
        fml = fml1 s
        p = p0 s

--sxを親に含むノード全部が家族
fml0::[(Int,Int)]->Int->[(Int,Int)]
fml0 s x = reverse $ sx:s'
    where
        (s',_) = break (\y -> fst y <= fst sx) left
        (sx:left) = reverse $ take (x+1) s

--sxを親に含むが、ある地点で区切る
fml1::[(Int,Int)]->Int->[(Int,Int)]
fml1 s x = reverse $ sx:s'
    where
        (s',_) = break (\y -> (fst y <= fst sx) || (snd y == 0 && fst y == fst sx + 1)) left
        (sx:left) = reverse $ take (x+1) s

--刈子の添字が0でないなら縦に上昇
tateVan::[(Int,Int)]->Int->[(Int,Int)]
tateVan ((x,y):s) n = van b d n ++ s
    where
        d = x - fst (s !! (r - 1))
        b = take r s
        r = root1 ((x,y):s)

root0でノードを辿り、
--そのノードに書いてある整数つまりペアの右の数を
--もとにヒドラを描き、root0でバッドルート探査。
root1::[(Int,Int)]->Int
root1 s = tobi !! root0 soe 0
    where
        soe = zip ([snd (s !! t) | t<-tobi]) (replicate s00 0)
        tobi = take s00 (iterate r0 0)
        s00 = 1 + fst (head s)
        r0 = root0 s

--dだけベアの左の数を加えながら、増殖させる
van::[(Int,Int)]->Int->Int->[(Int,Int)]
van _ _ 0 = []
van s d n = van s' d (n-1) ++ s'
    where s' = josyo s d

--左だけ増やすのを縮めただけ もっとうまく書けそう
josyo::[(Int,Int)]->Int->[(Int,Int)]
josyo s d = zip (map (+d) s0) s1
    where (s0,s1) = unzip s

−−バシク行列のp0と同じ、単に親
p0::[(Int,Int)]->Int->Int 
p0 _ (-1) = -1
p0 s x = if s0x == 0
    then -1
    else minimum [t | t <- [x..length s0 -1], s0 !! t < s0x]
    where 
        (s0,_) = unzip s
        s0x = s0 !! x

--展開しつつ角括弧の中を増やす感じ
keisan::[(Int,Int)]->Int->Int
keisan [] n = n
keisan s n = keisan (tenkai s n) n*n

--ハムペアの限界を召喚する
hamPair::Int->Int 
hamPair n = keisan s n
    where s = zip [0..n] [0..n]

--fを2n回合成した関数を返す
gousei :: (Int -> Int) -> Int -> (Int -> Int)
gousei f 0 = f
gousei f n = gousei (f . f) (n-1)

main::IO()
main = print $ gousei hamPair 43 86

