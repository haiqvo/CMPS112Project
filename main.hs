import System.IO  
import Control.Monad


data Troop = Sworder | Arrower deriving (Eq, Show, Read)

data Sworder = SworderCommander (Int, Int) | SworderTroop String deriving (Eq, Show, Read)

data Arrower = ArrowerCommander (Int, Int) | ArrowerTroop Int  deriving (Eq, Show, Read)

data Squad = SworderSquad [Sworder] | ArrowerSquad [Arrower] | ScatterSquad [Sworder] deriving (Eq, Show, Read)






main = do  
        handle <- openFile "test.txt" ReadMode
        mainloop handle sworder arrower scatterSworder
        hClose handle
        where 
            sworder = []
            arrower = []
            scatterSworder = []

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
