import Data.Char
import Data.List
import Data.Either

-- I.
numberFromTo :: Int -> Int -> [Int]
numberFromTo x y
     | x <= y = x : numberFromTo (x + 1) y
     | x > y = []
--II.
numberFromThenTo:: Int -> Int -> Int -> [Int]
numberFromThenTo x y z = if (x <= z)
                         then x : (numberFromThenTo y (2 * y - x) z)
                         else []

--III.
addLists :: [a] -> [a] -> [a]
addLists [] ly = ly
addLists lx [] = lx
addLists (xh:xt) ly = xh : addLists (xt) ly

--IV.
findPosition :: [a] -> Int -> a
findPosition [] _ = error "no elements"
findPosition (lh:lt) ele
    | ele == 0 = lh
    | ele /= 0 = findPosition (lt) (ele - 1)

--V.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (lh:lt) = reverseList lt ++ [lh]

--VI.
takeElems :: Int -> [a] -> [a]
takeElems _ [] = []
takeElems 0 lis = []
takeElems ele (lh:lt) = lh : takeElems (ele - 1) lt

--VII.
removeElems :: Int -> [a] -> [a]
removeElems _ [] = []
removeElems 0 lis = lis
removeElems ele (lh:lt) = removeElems (ele - 1) lt

--VIII.
zipToPair :: [a] -> [b] -> [(a,b)]
zipToPair [] list_y = []
zipToPair list_x [] = []
zipToPair (ax:ay) (bx:by) = (ax,bx) : zipToPair ay by

--IX.
replicateElem :: Int -> a -> [a]
replicateElem 0 _ = []
replicateElem ele rep = rep : replicateElem (ele - 1) rep

--X.
betweenElem :: a -> [a] -> [a]
betweenElem _ [] = []
betweenElem ele (lh:[]) = [lh]
betweenElem ele (lh:lt) = lh : ele : betweenElem ele lt

--XI.

--XII.
concatLists :: [[a]] -> [a]
concatLists [[]] = []
concatLists (lh:lt) = lh ++ concatLists lt

--XIII.
initials :: [a] -> [[a]]
initials [] = [[]]
initials (lh:lt) = initialsAcc [] (lh:lt)
                   where
                     initialsAcc rep [] = [rep]
                     initialsAcc rep (ih:it) = rep : initialsAcc (rep ++ [ih]) it

--XIV.
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (lh:lt) = tailsAcc (lh:lt) lt
                 where
                   tailsAcc rep [] = [rep, []]
                   tailsAcc rep (th:tl) = rep : tailsAcc (tail(rep)) (tl)
--XV.
heads :: Eq a => [[a]] -> [a]
heads [] = []
heads (lh:lt) = if (lh == [])
                then heads lt
                else head(lh) : heads lt

-- XVI.

total :: [[a]] -> Int
total [] = 0
total (h:t) = elemsInList h + total t

elemsInList :: [a] -> Int
elemsInList [] = 0
elemsInList (h:t) = 1 + elemsInList t

-- XVII.

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):t) = [(x,z)] ++ fun t

-- XVIII.

cola :: [(String, b, c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t


-- XIX.

idade :: Int -> Int -> [(String, Int)] -> [String]
idade _ _ [] = []
idade cd age ((name, bd):t) = if ((cd - bd) >= age)
                              then name : idade cd age (t)
                              else idade cd age (t)

-- XX.
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 0 = []
powerEnumFrom n 1 = [1]
powerEnumFrom n m = powerEnumFrom n (m-1) ++ [n^(m-1)]

-- XXII.

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (lh:lt) (ah:at) = if (lh == ah)
                           then prefixOf (lt) (at)
                           else False

-- XXV.
occursInList :: Eq a => a -> [a] -> [Int]
occursInList _ [] = []
occursInList n (lh:lt) = occursInListAcc n 0 (lh:lt)
                         where
                         occursInListAcc n acc [] = []
                         occursInListAcc n acc (ah:at) = if (n /= ah)
                                                         then occursInListAcc n (acc+1) at
                                                         else acc : occursInListAcc n (acc+1) at

-- XXVI.
noElemsRepeated :: Eq a => [a] -> [a]
noElemsRepeated [] = []
noElemsRepeated (lh:lt) = lh : noElemsRepeated (removeAllElems lh (lh:lt))

removeAllElems :: Eq a => a -> [a] -> [a]
removeAllElems _ [] = []
removeAllElems elem (ah:at)
   | elem /= ah = ah : removeAllElems elem (at)
   | otherwise = removeAllElems elem (at)

-- XVII.
deleteOneElem :: Eq a => a -> [a] -> [a]
deleteOneElem _ [] = []
deleteOneElem n (ah:at)
   | n == ah = at
   | otherwise = ah : deleteOneElem n at

-- XVIII.
removeFromList :: Eq a => [a] -> [a] -> [a]
removeFromList list [] = list
removeFromList [] _ = []
removeFromList (ah:at) (ra:rt)
   | ah == ra = removeFromList (at) (rt)
   | otherwise = ah : removeFromList (at) (ra:rt)
