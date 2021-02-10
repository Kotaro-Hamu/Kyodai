expand::[Int] -> Int -> [Int]
expand [] _ = []
expand ss n = reverse (expand' (reverse ss) n )

expand'::[Int]-> Int -> [Int]
expand' (0:ss) _ = ss
expand' ss n = concat (replicate n (tail b)) ++ g
    where
        (b,g) = familyBattle ss
        
familyBattle::[Int] -> ([Int],[Int])
familyBattle (s:ss) = (concat (f:strong),concat weak ++ s2)
    where
        (s1,s2) = break (s - 1 >) (s:ss)
        (strong,weak) = break (f >) fmlList
        (f:fmlList) = getFml s1 s

getFml::[Int] -> Int -> [[Int]]
getFml [] _ = []
getFml ss s = (xs++[x]):(getFml ys s)
    where
        (xs,x:ys) = break (s >) (ss)
