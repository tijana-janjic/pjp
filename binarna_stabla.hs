
--------- BINARNA STABLA  -----------

data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

-- da li se x nalazi u stablu
member :: Eq a => a -> Tree a -> Bool
member _ Empty = False
member x (Node l curr r) = x == curr || member x l || member x r

-- prebrojavanje cvorova
numNodes :: Tree a-> Int
numNodes Empty = 0
numNodes (Node l x r) = 1 + numNodes l + numNodes r 

-- obilasci stabla
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

-- obilazak po nivoima
bfs :: Tree a -> [a]
bfs Empty = []
bfs (Node l x r) = bfsAcc [x] [l, r]

-- bfsAcc nodes queue
bfsAcc nodes [] = nodes
bfsAcc nodes (q:qs) =
    case q of
        Empty -> bfsAcc nodes qs
        Node l x r -> bfsAcc (nodes ++ [x]) (qs ++ [l, r])


--------- BINARNA STABLA PRETRAGE -----------

-- ubacivanje cvora u BST
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty n = Node Empty n Empty
insert (Node l curr r) x
    | x == curr = (Node l curr r)
    | x < curr = (insert(Node l, x) curr r)
    | otherwise = (l curr insert(Node r, x))

-- formiranje BST na osnovu liste
formBST :: (Ord a) => [a] -> Tree a
formBST [] = Empty
formBST list =

formBST :: (Ord a) => [a] -> Tree a -> Tree a
formBSTRec [] tree = tree
formBSTRec [h|t] tree = formBSTAcc t (insert h tree)

memberBST :: Ord a => Tree a -> a -> Bool
memberBST Empty _ = False
memberBST (Node l curr r) x 
    | x == curr = True
    | x < curr = memberBST l x
    | otherwise = memberBST r x





