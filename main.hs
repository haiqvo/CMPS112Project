import System.IO  
import Control.Monad

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

data SworderSquad = SworderSquad SworderCommander [Sworder]
instance Squad SworderSquad where
    commander (SworderSquad x _) = x
    iter (SworderSquad x y) = y ++ x 

data ArrowerSquad = ArrowerSquad ArrowerCommander [Arrower]
instance Squad ArrowerSquad where
    commander (ArrowerSquad x _) = x
    iter (ArrowerSquad x y) = y ++ x

data ScatterSquad = ScatterSquad [Sworder]
instance Squad ScatterSquad where
    commander (ScatterSquad x _) = (SworderCommander (4, (Sworder 0)))
    iter (ScatterSquad x) = x

class Engagement x where 
    add :: Troop -> x


class EngageableGroup x where
    engageSwordVSword :: x -> x -> [Engagement]
    engageSwordVScatter :: x -> x -> [Engagement]
    engageScatterVSword :: x -> x -> [Engagement]
    engage :: (f -> b) -> [Engagement]


data SworderVSSworder = SworderVSSworder SworderSquad SworderSquad
instance EngageableGroup SworderVSSworder where
    engageSwordVSword (SworderSquad x) (SworderSquad y) = if (length x > length y)
                                                    then zip xs y
                                                         engageSwordVScatter zs
                                                         where
                                                            (xs, zs) = splitAt (length y) x 
                                                    else zip x ys 
                                                         engageScatterVSword as 
                                                         where
                                                            (ys, as) = splitAt (length x) y

    engageSwordVScatter (SworderSquad x) (ScatterSquad y) = if (length x > length y)
                                                    then zip xs y
                                                         engageSwordVScatter zs
                                                         where
                                                            (xs, zs) = splitAt (length y) x 
                                                    else zip x ys 
                                                         where
                                                            (ys, as) = splitAt (length x) y
    engageScatterVSword (ScatterSquad x) (SworderSquad y) = if (length x > length y)
                                                    then zip xs y
                                                         where
                                                            (xs, zs) = splitAt (length y) x 
                                                    else zip x ys 
                                                         engageScatterVSword as
                                                         where
                                                            (ys, as) = splitAt (length x) y
    engage (engageSwordVSword (SworderSquad x) (SworderSquad y)) = 


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
