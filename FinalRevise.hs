{-
s [] = []
s (x:xs) = (x*x):(s xs)


exprString :: Expr -> String -> String
exprString' :: Expr -> String -> String


nzpCount [] = (0,0,0)

nzpCount (x:xs) = let (n,z,p) = nzpCount xs in

 | x < 0 = (n+1,z,p)

 | x == 0 = (n,z+1,p)

 | otherwise = (n,z,p+1)
 
 
 
 fibs = 0:1:zipWith (+) fibs (tail fibs)
 
 
puts :: String -> IO ()
puts [] = return ()
puts (c:s) = putChar c >> puts s


data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord)

-- Define your types and functions below; do not edit above this line.
instance Show Suit where
    show Clubs = "C"
    show Diamonds = "D"
    show Hearts = "H"
    show Spades = "S"

data Bid = Pass | NoBid Int | Bid Suit Int
instance Show Bid where
    show Pass = "Pass"
    show (NoBid l) = (show l) ++ "NT"
    show (Bid s l) = (show l) ++ show s

toBid :: String -> Maybe Bid
toBid s = 
    case (filter (/=' ') s) of
        "Pass" -> Just Pass
        [l, s]-> do
            level <- toBidLevel l
            suit <- toBidSuit s
            return (Bid suit level)
        [l,'N', 'T'] -> do
            level <- toBidLevel l
            return (NoBid level)
        _->Nothing

bidSuit  :: Bid -> Maybe Suit
bidSuit Pass = Nothing
bidSuit (NoBid l) = Nothing
bidSuit (Bid s l) = Just s


bidLevel :: Bid -> Maybe Int
bidLevel Pass = Nothing
bidLevel (NoBid l)
    |l >= 0 && l <= 7 = Just l
    |otherwise = Nothing
bidLevel (Bid _ l)
    |l >= 0 && l <= 7 = Just l
    |otherwise = Nothing

toBidLevel :: Char -> Maybe Int
toBidLevel '1' = Just 1
toBidLevel '2' = Just 2
toBidLevel '3' = Just 3
toBidLevel '4' = Just 4
toBidLevel '5' = Just 5
toBidLevel '6' = Just 6
toBidLevel '7' = Just 7
toBidLevel _ = Nothing

toBidSuit :: Char -> Maybe Suit
toBidSuit 'C' = Just Clubs
toBidSuit 'D' = Just Diamonds
toBidSuit 'H' = Just Hearts
toBidSuit 'S' = Just Spades
toBidSuit _ = Nothing

--Q5:
type Coord = (Int,Int)

data Movement = North Int
              | South Int
              | East  Int
              | West  Int
              | Teleport Coord         

-- Write your finalPosition function here.  Do not change anything above this line.

finalPosition :: Coord -> [Movement] -> Coord
finalPosition (x,y) [] = (x,y)
finalPosition (x,y) (m:ms) = finalPosition (helper (x,y) m) ms

helper :: Coord -> Movement -> Coord
helper (x,y) (North n) = (x,y+n)
helper (x,y) (East n) = (x+n,y)
helper (x,y) (West n) = (x-n,y)
helper (x,y) (South n) = (x,y-n)


suffix :: Eq a => [a] -> [a] -> Maybe [a]
suffix xs ys
    | xs_length > ys_length = Nothing
    | all (==False) xs_check_list = Nothing
    | otherwise = Just $ drop ys_index ys
    where
        xs_length = length xs
        ys_length = length ys
        l = [0..(ys_length - xs_length)]
        xs_check_list = map (\x -> all (==True) (zipWith (==) (drop x ys) xs)) l
        ys_index = xs_length + length (takeWhile (/=True) xs_check_list)



suffix :: Eq a => [a] -> [a] -> Maybe [a]
suffix la@(x:xs) lb@(y:ys)
    | length la > length lb = Nothing
    | x == y && helper (x:xs) (y:ys) = Just $ drop (length la) lb
    | x /= y = suffix (x:xs) ys
    | otherwise = suffix (x:xs) ys
    
helper :: Eq a => [a] -> [a] -> Bool
helper (x:xs) (y:ys)
    | all (==True) $ zipWith (==) (x:xs) (y:ys) = True
    | otherwise = False


search :: (Int -> Bool) -> [Int] -> Int
search tst [] = error "search: no matching number found"
search tst (x:xs)
    | tst x     = x
    | otherwise = search tst xs
-}

as n = case (n `div` 2) of
     1 -> return 1
     2 -> return 2
