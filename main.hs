import System.IO  
import Control.Monad
import Data.Unique.Id

class Troop a where
    strength :: a -> Int 

data Sworder = Sworder Int
instance Troop Sworder where 
    strength (Sworder x) = x

data Arrower = Arrower Int
instance Troop Arrower where 
    strength (Arrower x) = x

class Commander c where
    command :: c -> Int
    strCommander :: c -> Int
    

data SworderCommander = SworderCommander (Int,Sworder) 
instance Commander SworderCommander where 
    command (SworderCommander x) = (fst x)
    strCommander (SworderCommander x) = strength (snd x)

data ArrowerCommander = ArrowerCommander (Int,Arrower) 
instance Commander ArrowerCommander where 
    command (ArrowerCommander x) = (fst x)
    strCommander (ArrowerCommander x) = strength (snd x)


class Squad c where 
    commander :: c -> Commander
    iter :: c -> [Troop]
    id :: c -> Id
    side :: c -> Int

data SworderSquad = SworderSquad SworderCommander [Sworder] Id Int 
instance Squad SworderSquad where
    commander (SworderSquad x _ _ _) = x
    iter (SworderSquad x y _ _) = y ++ x
    id (SworderSquad _ _ x _) = x
    side (SworderSquad _ _ _ x) = x 

data ArrowerSquad = ArrowerSquad ArrowerCommander [Arrower] Id Int 
instance Squad ArrowerSquad where
    commander (ArrowerSquad x _) = x
    iter (ArrowerSquad x y) = y ++ x
    id (ArrowerSquad _ _ x _) = x
    side (ArrowerSquad _ _ _ x) = x

data ScatterSquad = ScatterSquad Id Int [Sworder]  
instance Squad ScatterSquad where
    commander (ScatterSquad _ _ x _) = (SworderCommander (4, (Sworder 0)))
    iter (ScatterSquad _ _ x) = x
    id (ArrowerSquad _ x _) = x
    side (ArrowerSquad _ x _) = x

class Engagement x where 
    add :: Troop -> x


class EngageableGroup x where
    engageSwordVSword :: x -> x -> x -> x -> [Engagement]
    engageSwordVScatter :: x -> x -> x ->  [Engagement]
    engageWrap :: x -> x -> x -> [Engagement]


data SworderVSSworder = SworderVSSworder SworderSquad SworderSquad
instance EngageableGroup SworderVSSworder where
    engageSwordVSword ([SworderSquad] a@(x:xs)) ([SworderSquad] b@(y:ys)) ([ScatterSquad] c) ([ScatterSquad] d) = 
                                                if (length a > length b)
                                                    then (zip as b) ++ (engageSwordVScatter zs d b) 
                                                         where
                                                            (as, zs) = splitAt (length b) a 
                                                    else (zip a bs) ++ (engageSwordVScatter zs c a) 
                                                         where
                                                            (bs, zs) = splitAt (length a) b

    engageSwordVScatter ([SworderSquad] x) ([ScatterSquad] b@(y:ys)) ([SworderSquad] c) = 
                                                if (length x > length b)
                                                    then (zip xs b) ++ engageSwordVSword zs ys c
                                                         where
                                                            (xs, zs) = splitAt (length b) x 
                                                    else zip x bs

    engageWrap ([SworderSquad] a) ([ScatterSquad] b) ([SworderSquad] c) = 
                                                if (length a > length c)
                                                    then (zip as c) ++ engageSwordVScatter zs b c
                                                         where
                                                            (as, zs) = splitAt (length c) a
                                                    else zip a bs 


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
