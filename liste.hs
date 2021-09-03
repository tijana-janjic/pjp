quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort manjiJednaki ++ [x] ++ quicksort veci
    where
        manjiJednaki = filter (<= x) xs
        veci         = filter (> x) xs

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort neparni) (mergesort parni)
    where
        podijeli [] _ _ = []
        podijeli (x:xs) n o
            | mod n 2 == o  =   x : podijeli xs (n+1) o
            | otherwise     =   podijeli xs (n+1) o
        parni   = podijeli l 1 0
        neparni = podijeli l 1 1    
    
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge l []  = l
merge [] l  = l
merge (x:xs) (y:ys) 
    | x <= y     =   x : merge xs (y:ys)
    | otherwise =   y : merge (x:xs) ys    

mergesort' :: Ord a => [a] -> [a]
mergesort' [] = []
mergesort' [x] = [x]
mergesort' l = merge (mergesort' lijeva) (mergesort' desna)
    where 
        lijeva  =   take polovina l
        desna   =   drop polovina l
        polovina =   div (length l) 2

insertionSort :: Ord a => [a] -> [a]
insertionSort l = insertionSort' l []
    where 
        insertionSort' :: Ord a => [a] -> [a] -> [a]
        insertionSort' [] s     = s
        insertionSort' (x:xs) s = insertionSort' xs (insert x s)
        insert e [] = [e] 
        insert e (x:xs) 
            | e <= x    =   e : x : xs 
            | otherwise =   x : insert e xs



selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort l = minimum l : selectionSort (bezNajmanjeg l)
        where
            bezNajmanjeg (x:xs)
                | x == minimum l    =   xs
                | otherwise         =   x : bezNajmanjeg xs
