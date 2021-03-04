module Main where

import Lib
import Prelude hiding (return,sequence)

main :: IO ()
main = someFunc

bounce thunk = Doing thunk
return value = Done  value

data Thread a = Done a
              | Doing (Thread a)

factAcc :: Int -> Int -> Thread String
factAcc = \n acc ->
    if n == 0
    then return (show acc)
    else bounce (factAcc (pred n) (n * acc))

factAccCurry = \acc -> \n ->
    if n == 0
    then return (show acc)
    else bounce ((factAccCurry (n * acc)) (pred n))

mem :: Ord a => a -> [a] -> Thread String
mem = \n ls ->
    if null ls
    then return (show False)
    else if (head ls) == n
        then return (show True)
        else bounce (mem n (tail ls))

memCurry :: Ord a => [a] -> a -> Thread String
memCurry = \ls -> \n ->
    if null ls
    then return (show False)
    else if (head ls) == n
        then return (show True)
        else bounce ((memCurry (tail ls)) n)

pogoStick = \thread ->
    case thread of
        Done  value -> value
        Doing thunk -> pogoStick thunk

seesaw = \downThread upThread ->
    case downThread of
        Done  downValue -> downValue
        Doing downThunk -> seesaw upThread downThunk

trampoline = \threadQueue ->
    case head threadQueue of
        Done  value -> value
        Doing thunk -> trampoline ((tail threadQueue) ++ [thunk])

t1 = pogoStick (mem (pogoStick (factAcc 5 1)) ["100","110","120","130"])
t2 = sequence  (\value -> mem value ["100","110","120","130"]) (factAcc 5 1)
t3 = seqComp   (\value -> mem value ["100","110","120","130"]) (\x -> factAcc x 1)
t4 = seqComp   (memCurry ["100","110","120","130"]) (factAccCurry 1)

sequence :: (a -> Thread a) -> Thread a -> Thread a
sequence = \f thread ->
    case thread of
        Done  value -> f value
        Doing thunk -> bounce (sequence f thunk)

seqComp = \f g ->
    (\x -> sequence f (g x))


data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  

changeToP :: Directions-> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a  
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

data Direction = L | R deriving (Show)
type Directions = [Direction]

type Breadcrumbs a = [Crumb a]  

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)  

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)  

x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goUp (t, LeftCrumb x r:bs)  = (Node x t r, bs)  
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)  

type Zipper a = (Tree a, Breadcrumbs a)  

modify :: (a -> a) -> Zipper a -> Zipper a  
modify f (Node x l r, bs) = (Node (f x) l r, bs)  
modify f (Empty, bs) = (Empty, bs)  

attach :: Tree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs)  

topMost :: Zipper a -> Zipper a  
topMost (t,[]) = (t,[])  
topMost z = topMost (goUp z)  

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  

type ListZipper a = ([a],[a])  

goForward :: ListZipper a -> ListZipper a  
goForward (x:xs, bs) = (xs, x:bs)  
    
goBack :: ListZipper a -> ListZipper a  
goBack (xs, b:bs) = (b:xs, bs)  
