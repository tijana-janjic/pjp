-- Formira listu primenjujući ulaznu funkciju na sve elemente ulazne liste
-- definicija preko ZF izraza
mapZF :: (a -> b) -> [a] -> [b]
mapZF f xs = [f x | x <- xs]

-- rekurzivna definicija
mapR :: (a -> b) -> [a] -> [b]
mapR f [] = []
mapR f (x:xs) = f x : mapR f xs


-- Formira listu primenjujući ulaznu funkciju na sve elemente ulazne liste
-- definicija preko ZF izraza
filterZF :: (a -> Bool) -> [a] -> [a]
filterZF p xs = [x | x <- xs, p x]

-- rekurzivna definicija
filterR :: (a -> Bool) -> [a] -> [a]
filterR p [] = []
filterR p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- Redukuje elemente liste u jednu vrednost naspram nekog agregator operatora (funkcije dva argumenta)
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- provjerava da li svi elementi zadovoljavaju neki uslov
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]
all'' p []     = True
all'' p (x:xs) = p x && all'' p xs 

-- provjerava da li ijedan element zadovoljava neki uslov
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [p x | x <- xs]
any'' p []     = False
any'' p (x:xs) = p x || any'' p xs

-- uparuje dvije liste uz agregaciju
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- uzima elemente liste dok ne naidje na neki koji ne ispunjava uslov
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- sklanja elemente liste dok ne naidje na neki koji ne ispunjava uslov
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = (x:xs) 
