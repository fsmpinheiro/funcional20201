--Question 01
menorDeDois :: (Ord a) => a -> a -> a
menorDeDois x y = if x < y 
    then x 
    else y

--Question 02
--menorDeTres :: (Ord b) => b -> b -> b -> b
menorDeTres z y x
    | x < y && y < z = x
    | x > y && y < z = y
    | otherwise = z

--Question 03
fatorial :: (Eq c, Num c) => c -> c
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

--Question 04
fibonacci1 :: (Eq a, Num a, Num t) => a -> t
fibonacci1 0 = 0
fibonacci1 1 = 1
fibonacci1 n = fibonacci1 (n - 1) + fibonacci1 (n - 2)

--Question 05
nElemento _ [ ] = error "lista vazia seu vacilão!"
nElemento c xs =   drop c (take (c + 1) xs)

--nElemento1 _ [ ] = error "lista vazia seu vacilão!"
--nElemento1 c xs = 


--Question 06
pertence :: Eq t => t -> [t] -> Bool
pertence _ [ ] = False
pertence c (x:xs)
    | c == x = True
    | otherwise = pertence c xs


--Question 07
total :: Num a => [t] -> a
total xs = sum [ 1 | _ <- xs]
--total [] = 0
--total (x:xs) = x + (total xs) -- soma dos elementos da lista
--total (x:xs) = 1 + (total xs) -- quantidade de elementos na lista


--Question 08
--maior :: Ord a => [a] -> a
maior [ ] = error "lista vazia seu vacilão!"
maior [x] = x
maior (f:x:xs) = if f >= x 
        then maior (f:xs)
        else maior (x:xs)


--Question 09
frequencia _ [ ] = 0
frequencia u xs = sum [ 1 | x <- xs, x == u]


--Question 10
unico :: Int -> [Int] -> Bool
unico u xs 
    | sum [1 | x <- xs, x == u ] == 1 = True
    | otherwise = False


--Question 11
maioresQue :: Int -&gt; [Int] -&gt; [Int]
maioresQue u xs = [x | x &lt;- xs, x &gt; u]

--Question 12-a
concat0 [] [] = []
concat0 ys xs = ys ++ xs


--Question 12-b
concat1 a [] = a
concat1 [] b = b
concat1 a b = concat1 c (d : b)
        where c = init a
              d = last a


--Question 13
calda (x:xs) = xs
corpo xs = reverse (calda (reverse xs))


--Question 15
unique [] = []
unique [x] = [x]
unique (x:xs) = if x `elem` xs
        then unique xs
        else x : unique xs


--Question 16
menores :: Int -&gt; [Int] -&gt; [Int]
menores n xs = [ys | ys &lt;- xs, ys &lt; n ]


--Question 17
alter 1 = [1, (-1)]
alter a = alter (a - 1) ++ [a, (-a)]         


--Question 18
revers0 :: [a] -&gt; [a]
revers0 [ ] = [ ]
revers0 [x] = [x]
revers0 (x:xs) = (revers0 xs) ++ [x]


--Question 19
--dividO :: [a] -&gt; Int -&gt; ([a],[a,b])
dividO [] _ = ([],[])
dividO xs 0 = ([], xs)
dividO xs n = (a , b) 
        where 
            a = take n xs 
            b = drop n xs


--Question 20
intercal xs [ ] = xs
intercal [ ] ys = ys           
intercal (x:xs) ys = [x] ++ intercal ys xs


--Question 21
uniones [ ] ys = ys
uniones xs [ ] = xs
uniones (x:xs) (y:ys) = if y `elem` xs
    then uniones xs ys
    else y : uniones xs ys

--Question 22
interseciones [ ] ys = ys
interseciones xs [ ] = xs
interseciones xs ys = 
            let zs = [ a | a &lt;- xs, elem a ys] in [ b | b &lt;- ys, elem b zs]


--Question 23
sequencias n m = [z | z &lt;- [m..(m + (n-1)) ]] 


--Question 24-a
inserir0 n xs = r ++ [n] ++ t
    where r = takeWhile (&lt; n) xs
          t = dropWhile (&lt; n) xs


--Question 24-b
inserir1 n [ ] = [n]
inserir1 n (x:xs) =  if n &lt;= x
    then n:x:xs
    else x : inserir1 n xs


--Question 25
isSorteed [ ] = True
isSorteed [_] = True
isSorteed (x:n:xs) -- = if x &lt; n
    | x &lt;= n = isSorteed (n:xs)
    | otherwise = False
--    then = isSorteed n:xs
--    else = False


--Question 26
queSorted :: (Ord a) =&gt; [a] -&gt; [a]
queSorted [] = []
queSorted (x:xs) = a ++ [x] ++ b
    where a = queSorted [n | n &lt;- xs, n &lt;= x]
          b = queSorted [n | n &lt;- xs, n &gt; x]


--Question 31
selec1 [ ] _ = [ ]
selec1 ys xs = [ ys !! i | i &lt;- xs , i &gt;= 0 &amp;&amp; i &lt; length ys]

