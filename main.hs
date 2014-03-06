import System.IO  
import Control.Monad
import Data.Unique
import Data.Either

class Troop a where
    strength :: a -> Int 
    troopId :: a -> Int
    squadId :: a -> Int
    side :: a -> Int
    isCommander :: a -> Bool

data Sworder = Sworder Int Int Int Int Bool
instance Troop Sworder where 
    strength (Sworder x _ _ _ _) = x
    troopId (Sworder _ x _ _ _) = x
    squadId (Sworder _ _ x _ _) = x
    side (Sworder _ _ _ x _) = x
    isCommander (Sworder _ _ _ _ x) = x

data Arrower = Arrower Int Int Int Int Bool
instance Troop Arrower where 
    strength (Arrower x _ _ _ _) = x
    troopId (Arrower _ x _ _ _) = x
    squadId (Arrower _ _ x _ _) = x
    side (Arrower _ _ _ x _) = x
    isCommander (Arrower _ _ _ _ x) = x

class Commander c where
    command :: c -> Int
    strCommander :: c -> Int
    commanderId :: c -> Int


data SworderCommander = SworderCommander (Int,Sworder)
instance Commander SworderCommander where 
    command (SworderCommander x) = (fst x)
    strCommander (SworderCommander x) = strength (snd x)
    commanderId (SworderCommander x) = troopId (snd x)

data ArrowerCommander = ArrowerCommander (Int,Arrower) 
instance Commander ArrowerCommander where 
    command (ArrowerCommander x) = (fst x)
    strCommander (ArrowerCommander x) = strength (snd x)
    commanderId (ArrowerCommander x) = troopId (snd x)


class Squad c where 
    commander :: c -> Either ArrowerCommander SworderCommander
    iter :: c -> Either [Arrower] [Sworder]
    squadSide :: c -> Int
    squadIdToo :: c -> Int 


data SworderSquad = SworderSquad SworderCommander [Sworder] Int Int
instance Squad SworderSquad where
    commander (SworderSquad x _ _ _) = Right x
    iter (SworderSquad (SworderCommander x) y _ _) = Right (y ++ [(snd x)])
    squadSide (SworderSquad _ _ x _) = x 
    squadIdToo (SworderSquad _ _ _ x) = x

data ArrowerSquad = ArrowerSquad ArrowerCommander [Arrower] Int Int
instance Squad ArrowerSquad where
    commander (ArrowerSquad x _ _ _) = Left x
    iter (ArrowerSquad (ArrowerCommander x) y _ _) = Left (y ++ [(snd x)])
    squadSide (ArrowerSquad _ _ x _) = x
    squadIdToo (ArrowerSquad _ _ _ x) = x


data ScatterSquad = ScatterSquad Int Int [Sworder]  
instance Squad ScatterSquad where
    commander (ScatterSquad y x _) = Right (SworderCommander (4, (Sworder 0 0 x y)))
    iter (ScatterSquad _ _ x) = Right x
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



engageSwordVSword :: SworderSquad -> SworderSquad -> ScatterSquad -> ScatterSquad -> [([Sworder], [Sworder])] 
engageSwordVSword a b c d = 
                            if ((length sworder1) > (length sworder2))
                                then (zip as sworder2) ++ (engageSwordVScatter ys scatter2 sworder2)                   
                                else (zip sworder1 bs) ++ (engageSwordVScatter zs scatter1 sworder1) 
                            where
                                sworder1 = (iter a)
                                sworder2 = (iter b)
                                scatter1 = (iter c)
                                scatter2 = (iter d)
                                (as, ys) = splitAt (length (iter b)) (iter a)
                                (bs, zs) = splitAt (length (iter a)) (iter b)

engageSwordVScatter :: [Sworder] -> [Sworder] -> [Sworder] -> [([Sworder], [Sworder])]                                                
engageSwordVScatter a@([Sworder]) b@([Sworder]) c@([Sworder]) = -- (sworder)  (scatter)  (sworder)
                                                if (length a > length b)
                                                    then (zip as b) ++ engageWrap zs b c
                                                    else zip a b
                                                where
                                                    (as, zs) = splitAt (length b) a 
engageWrap :: [Sworder] -> [Sworder] -> [Sworder] -> [([Sworder], [Sworder])] 
engageWrap a@([Sworder]) b@([Sworder]) c@([Sworder]) =  -- (sworder)  (scatter)  (sworder)
                                                if (length a > length c)
                                                    then (zip as c) ++ engageSwordVScatter zs b c
                                                    else zip a c 
                                                where
                                                    (as, zs) = splitAt (length c) a

engageTroop :: [([Sworder], [Sworder])] -> [[(Sworder, Sworder)]]
engageTroop c@([([Sworder], [Sworder])]) = map ( \x ->
        if (length (fst x) > length (snd x))
            then let (as, ys) = splitAt (length (snd x)) (fst x)
                 in (zip as (snd x)) ++ (engageTroopWrap ys (snd x))
            else let (bs, zs) = splitAt (length (fst x)) (snd x)
                 in (zip (fst x) bs) ++ (engageTroopWrap zs (fst x))
        ) (engageSwordVSword c)

engageTroopWrap :: [Sworder] -> [Sworder] -> [[(Sworder, Sworder)]]
engageTroopWrap x@([Sworder]) y@([Sworder]) =
        if (length x > length y)
            then (zip xs y) ++ (engageTroopWrap zs y)
            else (zip x y)
        where 
            (xs, zs) = splitAt (length y) x

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
