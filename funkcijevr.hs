--Formira listu primenjujući ulaznu funkciju na sve elemente ulazne liste

-- definicija preko ZF izraza
mapZF :: (a -> b) -> [a] -> [b]
mapZF f xs = [f x | x <- xs]

-- rekurzivna definicija
mapR :: (a -> b) -> [a] -> [b]
mapR f [] = []
mapR f (x:xs) = f x : mapR f xs

-- definicija preko ZF izraza
filterZF :: (a -> Bool) -> [a] -> [a]
filterZF p xs = [x | x <- xs, p x]

-- rekurzivna definicija
filterR :: (a -> Bool) -> [a] -> [a]
filterR p [] = []
filterR p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- elementi liste su tipa a
-- lista se agregira u vrednost tipa b
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- elementi liste su tipa a
-- lista se agregira u vrednost tipa b
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs