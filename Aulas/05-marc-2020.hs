iguaiSS [ ] [ ] = True
iguaiSS (x:xs) [ ] = False
iguaiSS [ ] (y:ys) = False
iguaiSS (x:xs) (y:ys)
    | x == y = iguaiSS xs ys
    | otherwise = False


comparaTrue [ ] [ ] = "0"
comparaTrue  _  [ ] = "LT"
comparaTrue [ ]  _  = "GT"
comparaTrue (x:xs) (y:ys) 
    | x == y = comparaTrue xs ys
    | x < y = -1
    | otherwise = 1