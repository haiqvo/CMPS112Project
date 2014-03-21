--Kartoon Kombat
--This is a very basic version
--Hai Vo (hqvo@ucsc.edu)
--Richard Nicholson

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

-- So after so much tear were shed this program work in it very basic form and there was no time 
-- for a parser that takes from file sorry. So I already have a sample squad but to make more 
-- should have to know some things. 
-- (Sworder a b c d e f)
-- a is the strength this will determine the attack strength and health of the character
-- b is the troopId this is unique to the Sworder 
-- c is the squadId and it the same across Sworders of the same squad
-- d is the side and for squad1 please use 1 and squad two please use 2
-- e is the isCommander and should be zero unless it is the commander (note commander should be at the end of squads)
-- f is the damageTaken and should be zero at the beginning   
squad1 :: [[Sworder]]
squad1 = [[Sworder 4 2 1 1 0 0, Sworder 5 3 1 1 0 0, Sworder 6 4 1 1 0 0, Sworder 3 1 1 1 3 0],
          [Sworder 3 6 2 1 0 0, Sworder 2 7 2 1 0 0, Sworder 4 5 2 1 2 0],
          [Sworder 5 15 3 1 0 0, Sworder 4 16 3 1 1 0],
          [Sworder 10 17 6 1 0 0, Sworder 16 18 6 1 1 0],
          [Sworder 5 22 8 1 0 0, Sworder 7 23 8 1 0 0, Sworder 9 24 8 1 3 0]]


squad2 :: [[Sworder]]
squad2 = [[Sworder 9 11 4 2 0 0, Sworder 8 10 4 2 2 0], 
          [Sworder 7 13 5 2 0 0, Sworder 8 14 5 2 0 0, Sworder 12 12 5 2 3 0],
          [Sworder 4 19 7 2 0 0, Sworder 6 20 7 2 0 0, Sworder 8 21 7 2 2 0]]

--main loop that run the game
main = do mainloop (squad1, squad2)
        

--the loop that runs until a side wins
mainloop :: ([[Sworder]], [[Sworder]]) -> IO () 
mainloop (x, y) = 
    do print (x, y)
       let (a, b) = filterIntoSquad (filterSworder (reOrderTwoSide (convertToList (cycleTroops (engageSwordVSword x y [[]] [[]] ))) ([], [])))
       print ("----------Next Turn-----------")
       if null a
           then print "Squad 2 wins"
           else if null b 
                    then print "Squad 1 wins"
                    else mainloop (a, b)
                    


--Use to pair up the squads against other squad will also call engageWrap to wrap
--if there is too many squads on one side
engageSwordVSword :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])] 
engageSwordVSword sworder1 sworder2 scatter1 scatter2 = 
                            if ((length sworder1) > (length sworder2))
                                then (zip as sworder2) ++ (engageWrap ys scatter2 sworder2)                   
                                else (zip sworder1 bs) ++ (engageWrap zs scatter1 sworder1) 
                            where
                                (as, ys) = splitAt (length sworder2) sworder1
                                (bs, zs) = splitAt (length sworder1) sworder2

-- After the scatter have been pair any left over will wrap around to the normal squads again
engageWrap :: [[Sworder]] -> [[Sworder]] -> [[Sworder]] -> [([Sworder], [Sworder])] 
engageWrap largeSworderSquad scatter fullSworder = 
                                                if ((length largeSworderSquad) > (length fullSworder))
                                                    then (zip as fullSworder) ++ engageWrap zs scatter fullSworder
                                                    else zip largeSworderSquad fullSworder 
                                                where
                                                    (as, zs) = splitAt (length fullSworder) largeSworderSquad

--this pairs all of the troops than deals the damage which will go to the convertToList
cycleTroops :: [([Sworder], [Sworder])] -> [[(Sworder,Sworder)]]
cycleTroops [] = []
cycleTroops (x@(a,b):xs) = (cycleDamage (engageTroop x) (length a) (length b)) : cycleTroops xs

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

cycleDamage :: [(Sworder,Sworder)] -> Int -> Int -> [(Sworder,Sworder)]
cycleDamage [] _ _ = []
cycleDamage (x:xs) a b = (dealingDamageSVS a b x) : cycleDamage xs a b 

--Now that the squad is paired up it is time a dish out the damage to the all pairs 
-- it will be stored in the damage taken becasue multiplies enemy so the sworder may 
--appear more than once
dealingDamageSVS :: Int -> Int -> (Sworder, Sworder) -> (Sworder, Sworder)
dealingDamageSVS sworder1Defense sworder2Defense pairSworder = 
    ( takingDamage (fst pairSworder) (sworder2Attack) , takingDamage (snd pairSworder) (sworder1Attack))
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
                    if d == 1 
                        then reOrderTwoSide xs  ((z ++ [g]), y)
                        else reOrderTwoSide xs  (z, (y ++ [g]))

--So for both side filter all of the same troopId and add all of the damage taken 
filterSworder :: ([Sworder], [Sworder]) -> ([Sworder], [Sworder])
filterSworder (x, y) = (filterSworder2 x, filterSworder2 y)

--helper for filterSworder
filterSworder2 :: [Sworder]  -> [Sworder]
filterSworder2 [] = []
filterSworder2 (x@(Sworder a b c d e f):xs) = (foldl (\(Sworder a1 b1 c1 d1 e1 f1) (Sworder _ _ _ _ _ k) 
    -> (Sworder a1 b1 c1 d1 e1 (f1+k))) x (filter (\(Sworder _ b1 _ _ _ _) -> b == b1) xs)):
    (filterSworder2 (filter (\(Sworder _ b1 _ _ _ _) -> b /= b1) xs) )

--Now that all of the sworder have been filter the goal now is to filter the sworders into squad
-- this is the function that performs all of the damage and clearing of empty squads
filterIntoSquad :: ([Sworder], [Sworder]) -> ([[Sworder]], [[Sworder]])
filterIntoSquad x = (removeBlankSquad (removeDieSworder (calculateDamage (moveCommander (filterIntoSquad2 (fst x))))),
                     removeBlankSquad (removeDieSworder (calculateDamage (moveCommander (filterIntoSquad2 (snd x))))))

--helper for filterIntoSquad
filterIntoSquad2 :: [Sworder] -> [[Sworder]]
filterIntoSquad2 [] = []
filterIntoSquad2 w@(x@(Sworder _ _ c _ _ _):xs) = (filter (\(Sworder _ _ c1 _ _ _) -> c == c1) w) :
                                     filterIntoSquad2 (filter (\(Sworder _ _ c1 _ _ _) -> c /= c1) w) 

--So all for one side the goal is to find all of the commander and move them to the end of the squad
moveCommander :: [[Sworder]] -> [[Sworder]]
moveCommander [] = []
moveCommander (x:xs) = moveCommander2 x : moveCommander xs

--helper for the moveCommander 
moveCommander2 :: [Sworder] -> [Sworder]
moveCommander2 [] = []
moveCommander2 w@(x@(Sworder a b c d e f):xs) =  (filter (\(Sworder _ _ _ _ e1 _) ->  e1 == 0) w) ++ (filter (\(Sworder _ _ _ _ e1 _) ->  e1 /= 0) w)

--So once everything is filter the next step is the move the total takenDamage to the strength
calculateDamage :: [[Sworder]] -> [[Sworder]]
calculateDamage [] = []
calculateDamage (x:xs) = calculateDamage2 x : calculateDamage xs

--the helper for calculateDamage
calculateDamage2 :: [Sworder] -> [Sworder]
calculateDamage2 [] = []
calculateDamage2 (x@(Sworder a b c d e f):xs) = if (a-f) <= 0 
                                                    then (Sworder 0 b c d e 0) :  calculateDamage2 xs
                                                    else (Sworder (a-f) b c d e 0) : calculateDamage2 xs
--removing the die sworder (strength is 0) 
removeDieSworder :: [[Sworder]] -> [[Sworder]]
removeDieSworder [] = []
removeDieSworder (x:xs) = removeDieSworder2 x : removeDieSworder xs

--helper for removeDieSworder also check if the commander is dead
removeDieSworder2 :: [Sworder] -> [Sworder]
removeDieSworder2 [] = []
removeDieSworder2 w@(x@(Sworder a b c d e f):xs) = if a == 0
                                                        then if e /= 0
                                                            then [] 
                                                            else removeDieSworder2 xs 
                                                        else (Sworder a b c d e f) : removeDieSworder2 xs

--Check for blank squad and erase them from the list
removeBlankSquad :: [[Sworder]] -> [[Sworder]]
removeBlankSquad x =  (filter (not . null) x)

--combining back all of the scattered squad
combineScatterUnits :: [[Sworder]] -> [[Sworder]]
combineScatterUnits w@(x@(y@(Sworder a b c d e f):ys):xs) = (filter (\[(Sworder _ _ c1 _ _ _)] ->  c1 == 0) w)
 ++ (filter (\[(Sworder _ _ c2 _ _ _)] ->  c2 == 1) w) ++ (filter (\[(Sworder _ _ c1 _ _ _)] ->  (c1 == 1)
  && (c1 == 0) ) w)




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
