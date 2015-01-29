{--
 --  David Martín Alaminos
 --  LP, Q2 - 2013/2014
 --  Pràctica obligatòria de Haskell: implementació de k-d 2^n tree
 --
 --}



--
-- 1.
--

indexsAux :: (a -> Bool) -> [a] -> Int -> [Int]
indexsAux f [] _ = []
indexsAux f (x:xs) n
    | f x       = n:(indexsAux f xs (n+1))
    | otherwise = indexsAux f xs (n+1)

indexs :: (a -> Bool) -> [a] -> [Int]
indexs f l = indexsAux f l 0

binToInt :: [Int] -> Int
binToInt l = sum $ map (2^) $ (indexs (==1) (reverse l))

class Point p where
    sel             :: Int -> p -> Double
    dim             :: p -> Int
    childAux        :: p -> p -> [Int] -> [Int]
    child           :: p -> p -> [Int] -> Int
    dist            :: p -> p -> Double
    listToPoint     :: [Double] -> p
    ptrans          :: [Double] -> p -> p
    pscale          :: Double -> p -> p



--
-- 2.
--

newtype Point3d = Point3d (Double, Double, Double) deriving (Eq)

instance Point Point3d where
    sel n (Point3d (a,b,c))
        | n == 1    = a
        | n == 2    = b
        | otherwise = c

    dim _ = 3

    childAux p1 p2 [] = []
    childAux p1 p2 (x:xs)
        | (sel x p1) > (sel x p2) = 1:(childAux p1 p2 xs)
        | otherwise                 = 0:(childAux p1 p2 xs)

    child p1 p2 l =
        binToInt (childAux p1 p2 l)

    dist (Point3d (x,y,z)) (Point3d (a,b,c)) =
        sqrt ((a-x)^2 + (b-y)^2 + (c-z)^2)

    listToPoint (x:xs) = (Point3d (x, head xs, head (tail xs)))::Point3d   --cal fer " ::Point3d" sobre la llista d'entrada

    ptrans (a:(b:(c:cs))) (Point3d (x,y,z)) =
        (Point3d (a+x,b+y,c+z))

    pscale n (Point3d (x,y,z)) =
        (Point3d (n*x,n*y,n*z))



--
-- 3.
--

data Kd2nTree t = Empty | Kd2nTree t [Int] ([Kd2nTree t])

getPointList :: Point a => [Kd2nTree a] -> [(a, [Int])]
getPointList [] = []
getPointList [Empty] = []
getPointList [(Kd2nTree p1 c1 [])] = [(p1,c1)]
getPointList ((Kd2nTree p1 c1 x):[]) = [(p1,c1)]++(getPointList x)
getPointList ((Empty):ls) =
    (getPointList ls)
getPointList ((Kd2nTree p1 c1 x):ls) =
    [(p1,c1)]++(getPointList x)++(getPointList ls)

eqKd2nTree :: Point a => Eq a => (Kd2nTree a) -> (Kd2nTree a) -> Bool
eqKd2nTree t1 t2 =
    [x | x <- la, not (elem x lb)] == [x | x <- lb, not (elem x la)]
    where
        la = [l | (l,r) <- (getPointList [t1])]
        lb = [l | (l,r) <- (getPointList [t2])]

instance (Point a, Eq a) => Eq (Kd2nTree a) where
    t1 == t2    = eqKd2nTree t1 t2

instance Show (Point3d) where
    show (Point3d (x,y,z)) = "("++(show x)++","++(show y)++","++(show z)++")"

showChilds :: Point a => Show a => [(Kd2nTree a)] -> Int -> Int -> [Char]
showChilds [] n s = ""
showChilds ((Empty):xs) n s = (showChilds xs (n+1) s)
showChilds (x:xs) n s =
    "\n"++(concat (take (s) (repeat " ")))++"<"++(show n)++"> "++(showKd2nTree x s)++(showChilds xs (n+1) s)

showKd2nTree :: Point a => Show a => (Kd2nTree a) -> Int -> [Char]
showKd2nTree Empty _ = ""
showKd2nTree (Kd2nTree p l []) s = (show p)++" "++(show l)
showKd2nTree (Kd2nTree p l (x:xs)) s =
    (show p)++" "++(show l)++(showChilds (x:xs) 0 (s+4))

instance (Point a, Show a) => Show (Kd2nTree a) where
    show t = showKd2nTree t 0



--
-- 4.
--

fillEmpty :: Int -> [(Kd2nTree a)]
fillEmpty n =
    take (2^n) (repeat Empty)

insert :: Point a => Kd2nTree a -> a -> [Int] -> Kd2nTree a
insert Empty pt l = (Kd2nTree pt l (fillEmpty (length l)))
insert (Kd2nTree p lc chi) pt l =
    (Kd2nTree p lc ((fst part) ++ [(insert (chi !! (child pt p lc)) pt l)] ++ (tail (snd part))) )
    where
        part = (splitAt (child pt p lc) chi)

buildAux :: Point a => Kd2nTree a -> [(a, [Int])] -> Kd2nTree a
buildAux t [] = t
buildAux t ((p,l):xs) =
    buildAux (insert t p l) xs

build :: Point a => [(a, [Int])] -> Kd2nTree a
build [] = Empty
build l =
    buildAux Empty l

buildIni :: Point a => [([Double], [Int])] -> Kd2nTree a  -- cal envoltar la crida amb "::(Kd2nTree Point3d)"
buildIni [] = Empty
buildIni ll =
    buildAux Empty lt
    where lt = [((listToPoint l),r) | (l,r) <- ll]



--
-- 5.
--

get_all :: Point a => Kd2nTree a -> [(a, [Int])]
get_all t = getPointList [t]



--
-- 6.
--

remove :: Eq a => Point a => Kd2nTree a -> a -> Kd2nTree a
remove Empty _ = Empty
remove (Kd2nTree p lc chi) pt
    | p == pt   = build (getPointList chi)
    | otherwise = (Kd2nTree p lc ((fst part)++[(remove (chi !! (child pt p lc)) pt)]++(tail (snd part))))
    where
        part = (splitAt (child pt p lc) chi)



--
-- 7.
--

contains :: Eq a => Point a => Kd2nTree a -> a -> Bool
contains Empty _ = False
contains (Kd2nTree p lc chi) pt
    | p == pt   = True
    | otherwise = (contains (chi !! (child pt p lc)) pt)



--
-- 8.
--

nearestAux :: Eq a => Point a => [Kd2nTree a] -> a -> a -> a
nearestAux [] cand pt = cand
nearestAux ((Empty):xs) cand pt = nearestAux xs cand pt
nearestAux ((Kd2nTree p lc chi):xs) cand pt
    | (dist p pt) >= (dist cand pt) = nearestAux xs cand pt
    | otherwise                     = nearestAux (chi++xs) p pt

nearest :: Eq a => Point a => Kd2nTree a -> a -> a
nearest (Kd2nTree p lc chi) pt =
    nearestAux chi p pt



--
-- 9.
--

sameSign :: Point p => Point q => p -> q -> Int -> Bool
sameSign _ _ (-1) = True
sameSign p q n
    | (sel n p) >= 0 && (sel n q) >= 0  = sameSign p q (n-1)
    | (sel n p) < 0 && (sel n q) < 0    = sameSign p q (n-1)
    | otherwise                                 = False

isClosedOp :: Point p => Point q => p -> q -> Int -> (Double -> Double -> Double) -> Double -> Bool
isClosedOp _ _ (-1) _ _ = True
isClosedOp p q d op k
    | ((op (sel d p) k) == (sel d q))   = (isClosedOp p q (d-1) op k)
    | otherwise                         = False

kd2nMap :: Point p => Point q => (p -> q) -> [Kd2nTree p] -> [Kd2nTree q]
kd2nMap f tl =
    [(Kd2nTree (f p) lc (kd2nMap f chi)) | (Kd2nTree p lc chi) <- tl]

kdmap :: Point p => Point q => (p -> q) -> Kd2nTree p -> Kd2nTree q
kdmap f (Kd2nTree p lc chi)
    | (isClosedOp p (f p) mind (+) ((sel 0 (f p)) - (sel 0 p))) && (sameSign p (f p) mind)  = head (kd2nMap f [(Kd2nTree p lc chi)])
    | (isClosedOp p (f p) mind (-) ((sel 0 (f p)) + (sel 0 p))) && (sameSign p (f p) mind)  = head (kd2nMap f [(Kd2nTree p lc chi)])
    | (isClosedOp p (f p) mind (*) ((sel 0 (f p)) / (sel 0 p))) && (sameSign p (f p) mind)  = head (kd2nMap f [(Kd2nTree p lc chi)])
    | (isClosedOp p (f p) mind (/) ((sel 0 (f p)) * (sel 0 p))) && (sameSign p (f p) mind)  = head (kd2nMap f [(Kd2nTree p lc chi)])
    | otherwise     = build [((f a),b) | (a,b) <- getPointList [(Kd2nTree p lc chi)]]
    where mind = min (dim p) (dim (f p))

translation :: Point p => [Double] -> Kd2nTree p -> Kd2nTree p
translation lt tr =
    kdmap (\p -> (ptrans lt p)) tr

scale :: Point p => Double -> Kd2nTree p -> Kd2nTree p
scale n tr =
    kdmap (\p -> (pscale n p)) tr





--
-- Joc de proves de l'enunciat
--

exampleSet :: Kd2nTree Point3d
exampleSet = Kd2nTree (Point3d (3.0,(-1.0),2.1)) [1,3] [example0,example3,example4,example5]

example0 :: Kd2nTree Point3d
example0 = Kd2nTree (Point3d (3.0,5.1,0.0)) [2] [example1,example2]

example1 :: Kd2nTree Point3d
example1 = Kd2nTree (Point3d (1.8,1.1,(-2.0))) [1,2] [Empty,Empty,Empty,Empty]

example2 :: Kd2nTree Point3d
example2 = Kd2nTree (Point3d (1.5,8.0,1.5)) [1] [Empty,Empty]

example3 :: Kd2nTree Point3d
example3 = Kd2nTree (Point3d (3.0,(-1.7),3.1)) [1,2,3] [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]

example4 :: Kd2nTree Point3d
example4 = Kd2nTree (Point3d (3.5,0.0,2.1)) [3] [Empty,Empty]

example5 :: Kd2nTree Point3d
example5 = Kd2nTree (Point3d (3.5,2.8,3.1)) [1,2] [example6,example7,Empty,example8]

example6 :: Kd2nTree Point3d
example6 = Kd2nTree (Point3d (3.3,2.8,2.5)) [3] [Empty,Empty]

example7 :: Kd2nTree Point3d
example7 = Kd2nTree (Point3d (3.1,3.8,4.8)) [1,3] [Empty,Empty,Empty,Empty]

example8 :: Kd2nTree Point3d
example8 = Kd2nTree (Point3d (4.0,5.1,3.8)) [2] [Empty,Empty]

example9 :: Kd2nTree Point3d
example9 = Kd2nTree (Point3d (4.0,5.1,3.8)) [2] [Empty,Empty]
