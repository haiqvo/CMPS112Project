import System.IO  
import Control.Monad
import Data.Unique
import Data.Either

class Troop a where
    strength :: a -> Int 
    troopId :: a -> Int
    squadId :: a -> Int
    side :: a -> Int
    isCommander :: a -> Int 

data Sworder = Sworder Int Int Int Int Int
instance Troop Sworder where 
    strength (Sworder x _ _ _ _) = x
    troopId (Sworder _ x _ _ _) = x
    squadId (Sworder _ _ x _ _) = x
    side (Sworder _ _ _ x _) = x
    isCommander (Sworder _ _ _ _ x) = x

data Arrower = Arrower Int Int Int Int Int
instance Troop Arrower where 
    strength (Arrower x _ _ _ _) = x
    troopId (Arrower _ x _ _ _) = x
    squadId (Arrower _ _ x _ _) = x
    side (Arrower _ _ _ x _) = x
    isCommander (Arrower _ _ _ _ x) = x


class Squad c where 
    iter :: Troop c => c -> c  
    squadSide :: c -> Int
    squadIdToo :: c -> Int 


data SworderSquad = SworderSquad [Sworder] Int Int
instance Squad SworderSquad where
    iter (SworderSquad y _ _) = y
    squadSide (SworderSquad  _ x _) = x 
    squadIdToo (SworderSquad _ _ x) = x

data ArrowerSquad = ArrowerSquad [Arrower] Int Int
instance Squad ArrowerSquad where
    iter (ArrowerSquad y _ _) = y 
    squadSide (ArrowerSquad  _ x _) = x
    squadIdToo (ArrowerSquad _ _ x) = x


data ScatterSquad = ScatterSquad Int Int [Sworder]  
instance Squad ScatterSquad where
    iter (ScatterSquad _ _ x) =  x
    squadSide (ScatterSquad x _ _) = x
    squadIdToo (ScatterSquad _ x _) = x


    

main = print "test"
        --let sworder = SworderTroop 5
        --print (strength sworder)
        --handle <- openFile "test.txt" ReadMode
        --mainloop handle sworder arrower scatterSworder
        --hClose handle
        

mainloop :: Handle -> [String] -> [String] -> [String] -> IO () 
mainloop inh sworder arrower scatterSworder = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   let list' = parseUnits inpStr sworder arrower scatterSworder
                   print (head list')
                   mainloop inh sworder arrower scatterSworder



engageSwordVSword :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])] 
engageSwordVSword sworder1 sworder2 scatter1 scatter2 = 
                            if ((length sworder1) > (length sworder2))
                                then (zip as sworder2) ++ (engageSwordVScatter ys scatter2 sworder2)                   
                                else (zip sworder1 bs) ++ (engageSwordVScatter zs scatter1 sworder1) 
                            where
                                (as, ys) = splitAt (length sworder2) sworder1
                                (bs, zs) = splitAt (length sworder1) sworder2

engageSwordVScatter :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])]                                                
engageSwordVScatter largeSworderSquad scatter fullSworder = -- (sworder)  (scatter)  (sworder)
                                                if (length largeSworderSquad > length scatter)
                                                    then (zip as scatter) ++ engageWrap zs scatter fullSworder
                                                    else zip largeSworderSquad scatter
                                                where
                                                    (as, zs) = splitAt (length scatter) largeSworderSquad 

engageWrap :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])] 
engageWrap largeSworderSquad scatter fullSworder =  -- (sworder)  (scatter)  (sworder)
                                                if (length largeSworderSquad > length scatter)
                                                    then (zip as scatter) ++ engageSwordVScatter zs scatter fullSworder
                                                    else zip largeSworderSquad fullSworder 
                                                where
                                                    (as, zs) = splitAt (length fullSworder) largeSworderSquad

engageTroop :: [([Sworder], [Sworder])] -> [(Sworder, Sworder)]
engageTroop [] = []
engageTroop (x:xs) = (zipOverFlow (fst x) (snd x)) ++ (engageTroop xs)


zipOverFlow :: [Sworder] -> [Sworder] -> [(Sworder, Sworder)]
zipOverFlow x y = if (length x > length y) 
                     then let (xs, zs) = splitAt (length y) x
                          in (zip xs y) ++ zipOverFlowHelper zs y
                     else let (ys, zs) = splitAt (length x) y
                          in (zip x ys) ++ zipOverFlowHelper zs x

zipOverFlowHelper ::  [Sworder] -> [Sworder] -> [(Sworder, Sworder)]
zipOverFlowHelper large small = 
    if (length large > length small) 
        then let (xs, zs) = splitAt (length small) large
             in (zip xs small) ++ zipOverFlowHelper zs small
        else zip large small






parseUnits :: [Char] -> [String] -> [String] -> [String] -> [String]
parseUnits (x:y:zs) a b c = 
    if x == '|'
        then a ++ (split ',' zs)
        else if x == '!' 
            then b ++ (split ',' zs)
            else if x == '~'
                then c ++ (split ',' zs)
                else []


split :: Eq a => a -> [a] -> [[a]]
split x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if y==x 
                                  then func x ys ([]:(z:zs)) 
                                  else func x ys ((y:z):zs)
