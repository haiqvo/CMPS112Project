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
    damageTaken :: a -> Int

--sworder type 
data Sworder = Sworder Int Int Int Int Int Int deriving Show
instance Troop Sworder where 
    strength (Sworder x _ _ _ _ _) = x
    troopId (Sworder _ x _ _ _ _) = x
    squadId (Sworder _ _ x _ _ _) = x
    side (Sworder _ _ _ x _ _) = x
    isCommander (Sworder _ _ _ _ x _) = x
    damageTaken (Sworder _ _ _ _ _ x) = x

--arrower type
data Arrower = Arrower Int Int Int Int Int Int deriving Show
instance Troop Arrower where 
    strength (Arrower x _ _ _ _ _) = x
    troopId (Arrower _ x _ _ _ _) = x
    squadId (Arrower _ _ x _ _ _) = x
    side (Arrower _ _ _ x _ _) = x
    isCommander (Arrower _ _ _ _ x _) = x
    damageTaken (Arrower _ _ _ _ _ x) = x

 
--data Commander a = Commander (Int, a)

--class Squad t c where
--    commander :: c -> Commander t
--    iter :: c -> [t]

main = do handle <- openFile "test.txt" ReadMode
          mainloop handle
          hClose handle
        

mainloop :: Handle -> IO () 
mainloop inh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do  inpStr <- hGetLine inh
                    mainloop inh
                    


--Use to pair up the squads against other squad
engageSwordVSword :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])] 
engageSwordVSword sworder1 sworder2 scatter1 scatter2 = 
                            if ((length sworder1) > (length sworder2))
                                then (zip as sworder2) ++ (engageSwordVScatter ys scatter2 sworder2)                   
                                else (zip sworder1 bs) ++ (engageSwordVScatter zs scatter1 sworder1) 
                            where
                                (as, ys) = splitAt (length sworder2) sworder1
                                (bs, zs) = splitAt (length sworder1) sworder2

--Called by engageSwordVSword after all of the smaller side of has been pair
engageSwordVScatter :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])]                                                
engageSwordVScatter largeSworderSquad scatter fullSworder = 
                                                if (length largeSworderSquad > length scatter)
                                                    then (zip as scatter) ++ engageWrap zs scatter fullSworder
                                                    else zip largeSworderSquad scatter
                                                where
                                                    (as, zs) = splitAt (length scatter) largeSworderSquad 

-- After the scatter have been pair any left over will wrap around to the normal squads again
engageWrap :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])] 
engageWrap largeSworderSquad scatter fullSworder = 
                                                if (length largeSworderSquad > length scatter)
                                                    then (zip as scatter) ++ engageSwordVScatter zs scatter fullSworder
                                                    else zip largeSworderSquad fullSworder 
                                                where
                                                    (as, zs) = splitAt (length fullSworder) largeSworderSquad

--After the squads have been paired up its time to pair up the sworder
engageTroop :: ([Sworder], [Sworder]) -> [(Sworder, Sworder)]
engageTroop (x) = (zipOverFlow (fst x) (snd x))

--Warp around the function when there is a larger squad
zipOverFlow :: [Sworder] -> [Sworder] -> [(Sworder, Sworder)]
zipOverFlow x y = if (length x > length y) 
                     then let (xs, zs) = splitAt (length y) x
                          in (zip xs y) ++ zipOverFlowHelper zs y
                     else let (ys, zs) = splitAt (length x) y
                          in (zip x ys) ++ zipOverFlowHelper zs x

--wrap around helper
zipOverFlowHelper ::  [Sworder] -> [Sworder] -> [(Sworder, Sworder)]
zipOverFlowHelper large small = 
    if (length large > length small) 
        then let (xs, zs) = splitAt (length small) large
             in (zip xs small) ++ zipOverFlowHelper zs small
        else zip large small

--Now that the squad is paired up it is time a dish out the damage to the all pairs 
-- it will be stored in the damage taken becasue multiplies enemy so the sworder may 
--appear more than once
dealingDamageSVS :: Int -> Int -> (Sworder, Sworder) -> (Sworder, Sworder)
dealingDamageSVS sworder1Defense sworder2Defense pairSworder = 
    ( takingDamage (fst pairSworder) (sworder1Health - sworder2Attack) , takingDamage (snd pairSworder) (sworder2Health - sworder1Attack))
        where 
            sworder1Health = strength (fst pairSworder)
            sworder2Health = strength (snd pairSworder)
            sworder1Attack = (((strength (fst pairSworder)) `div` sworder2Defense) + 1)
            sworder2Attack = (((strength (snd pairSworder)) `div` sworder1Defense) + 1)

--helper function for dealingDamageSVS use to store in the damage taken 
takingDamage :: Sworder -> Int -> Sworder
takingDamage x y = Sworder (strength x) (troopId x) (squadId x) (side x) (isCommander x) ((damageTaken x) + y)

--Once all the damage is taken it will take all of the sworders and in battle and put them all 
--in once big list
convertToList :: [[(Sworder, Sworder)]] -> [Sworder]
convertToList [] = []
convertToList (x:xs) = convertToList2 x ++ convertToList xs

--helper function for convertToList
convertToList2 :: [(Sworder, Sworder)] -> [Sworder]
convertToList2 [] = []
convertToList2 (x:xs) = [(fst x)] ++ [(snd x)] ++ convertToList2 xs

--From the big list now filter them into two sides
reOrderTwoSide :: [Sworder] -> ([Sworder], [Sworder]) -> ([Sworder], [Sworder])
reOrderTwoSide [] x = x
reOrderTwoSide (g@(Sworder a b c d e f):xs) (z, y) = 
                    if d == 0 
                        then reOrderTwoSide xs  ((z ++ [g]), y)
                        else reOrderTwoSide xs  (z, (y ++ [g]))

--So for both side filter all of the same troopId and add all of the damage taken 
filterSworder :: ([Sworder], [Sworder]) -> ([Sworder], [Sworder])
filterSworder (x, y) = (filterSworder2 x, filterSworder2 y)

--helper for filterSworder
filterSworder2 :: [Sworder]  -> [Sworder]
filterSworder2 [] = []
filterSworder2 (x@(Sworder a b c d e f):xs) = (foldl (\(Sworder a1 b1 c1 d1 e1 f1) (Sworder _ _ _ _ _ k) -> (Sworder a1 b1 c1 d1 e1 (f1+k))) x (filter (\(Sworder _ b1 _ _ _ _) -> b == b1) xs)):(filterSquad2 xs)

--Now that all of the sworder have been filter the goal now is to filter the sworders into squad
filterIntoSquad :: ([Sworder], [Sworder]) -> ([[Sworder]], [[Sworder]])
filterIntoSquad x = (filterIntoSquad2 (fst x), filterIntoSquad2 (snd x))

--helper for filterIntoSquad
filterIntoSquad2 :: [Sworder] -> [[Sworder]]
filterIntoSquad2 [] = []
filterIntoSquad2 w@(x@(Sworder _ _ c _ _ _):xs) = (filter (\(Sworder _ _ c1 _ _ _) -> c == c1) w) : filterIntoSquad2 xs 

--So all for one side the goal is to find all of the commander and move them to the end of the squad
moveCommander :: [[Sworder]] -> [[Sworder]]
moveCommander (x:xs) = moveCommander2 x : moveCommander xs

--helper for the moveCommander 
moveCommander2 :: [Sworder] -> [Sworder]
moveCommander2 [] = []
moveCommander2 w@(x@(Sworder a b c d e f):xs) =  (filter (\(Sworder _ _ _ _ e1 _) ->  e1 == 0) w) ++ (filter (\(Sworder _ _ _ _ e1 _) ->  e1 /= 0) w)

--So once everything is filter the next step is the move the total takenDamage to the strength
calculateDamage :: [[Sworder]] -> [[Sworder]]
calculateDamage (x:xs) = calculateDamage2 x : calculateDamage xs

--the helper for calculateDamage
calculateDamage2 :: [Sworder] -> [Sworder]
calculateDamage2 [] = []
calculateDamage2 (x@(Sworder a b c d e f):xs) = if (a-f) <= 0 
                                                    then (Sworder 0 b c d e 0) :  calculateDamage2 xs
                                                    else (Sworder (a-f) b c d e 0) : calculateDamage2 xs
--removing the die sworder (strength is 0) 
removeDieSworder :: [[Sworder]] -> [[Sworder]]
removeDieSworder (x:xs) = removeDieSworder2 x : removeDieSworder xs

--helper for removeDieSworder also check if the commander is dead
removeDieSworder2 :: [Sworder] -> [Sworder]
removeDieSworder2 [] = []
removeDieSworder2 w@(x@(Sworder a b c d e f):xs) = if a == 0
                                                        then if e /= 0
                                                            then destorySquad w 
                                                            else removeDieSworder2 xs 
                                                        else (Sworder a b c d e f) : removeDieSworder2 xs

--destorying the squad if the commander is dead
destorySquad :: [Sworder] -> [Sworder]
destorySquad [] = []
destorySquad w@(x@(Sworder a b c d e f):xs) = if d == 0 
                                                then (Sworder a b 0 d e f) : destorySquad xs
                                                else (Sworder a b 1 d e f) : destorySquad xs

--combining back all of the scattered squad
combineScatterUnits :: [[Sworder]] -> [[Sworder]]
combineScatterUnits w@(x@(y@(Sworder a b c d e f):ys):xs) = (filter (\[(Sworder _ _ c1 _ _ _)] ->  c1 == 0) w) ++ (filter (\[(Sworder _ _ c2 _ _ _)] ->  c2 == 1) w) ++ (filter (\[(Sworder _ _ c1 _ _ _)] ->  (c1 == 1) && (c1 == 0) ) w)




parseUnits :: [Char] -> [String] -> [String]
parseUnits (x:y:zs) a = 
    if x == '|'
        then a ++ (split ',' zs)
        else if x == '!' 
            then a ++ (split ',' zs)
            else if x == '~'
                then a ++ (split ',' zs)
                else []


split :: Eq a => a -> [a] -> [[a]]
split x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if y==x 
                                  then func x ys ([]:(z:zs)) 
                                  else func x ys ((y:z):zs)
