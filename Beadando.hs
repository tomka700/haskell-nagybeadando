module Beadando where

showState a = show a
showMage a = show a
eqMage a b =  a == b
showUnit a = show a
showOneVOne a = show a 

type Name = String
type Health = Integer
type Spell = (Integer -> Integer)
type Army = [Unit]
type EnemyArmy = Army
type Amount = Integer
--1
data State a = Alive a | Dead
    deriving (Eq)

instance (Show a) => Show (State a) where
    show (Alive x) = show x
    show _ = "Dead"

data Entity = Golem Health | HaskellElemental Health
    deriving (Show, Eq)

data Mage = Master Name Health Spell

instance Show Mage where
    show (Master x y _)
        | y < 5 = "Wounded " ++ x
        | otherwise = x

instance Eq Mage where
    (Master a b _) == (Master x y _) = a == x && b == y

papi = let 
    tunderpor enemyHP
        | enemyHP < 8 = 0
        | even enemyHP = div (enemyHP * 3) 4
        | otherwise = enemyHP - 3
    in Master "Papi" 126 tunderpor
java = Master "Java" 100 (\x ->  x - (mod x 9))
traktor = Master "Traktor" 20 (\x -> div (x + 10) ((mod x 4) + 1))
jani = Master "Jani" 100 (\x -> x - div x 4)
skver = Master "Skver" 100 (\x -> div (x+4) 2)

data Unit = M (State Mage) | E (State Entity)
    deriving (Eq)

instance Show Unit where
    show (M x) = show x
    show (E x) = show x
--2
formationFix :: Army -> Army
formationFix l = [x | x <- l, hp x /= 0] ++ [x | x <- l, hp x == 0]

hp :: Unit -> Health
hp (E (Alive (Golem x))) = x
hp (E (Alive (HaskellElemental x))) = x
hp (M (Alive (Master _ x _))) = x
hp _ = 0
--3
over :: Army -> Bool
over = all $ \x -> hp x == 0

potionMaster = 
  let plx x
        | x > 85  = x - plx (div x 2)
        | x == 60 = 31
        | x >= 51 = 1 + mod x 30
        | otherwise = x - 7 
  in Master "PotionMaster" 170 plx
--4
fight :: EnemyArmy -> Army -> Army
fight _ [] = []
fight [] l = l
fight ((E (Alive (Golem (_)))):xs) (y:ys) = hit (+ (-1)) y : fight xs ys
fight ((E (Alive (HaskellElemental (_)))):xs) (y:ys) = hit (+ (-3)) y : fight xs ys
fight ((M (Alive (Master _ _ f))):xs) (y:ys) = hit f y : fight xs (map (hit f) ys)
fight (x:xs) (y:ys) = y : fight xs ys -- hp x == 0

hit :: Spell -> Unit -> Unit --pozitiv noveli a hpt
hit f (M (Alive (Master a x c)))
    | f x > 0 = M (Alive (Master a (f x) c))
    | otherwise = M Dead
hit f (E (Alive (Golem x)))
    | f x > 0 = E (Alive (Golem $ f x))
hit f (E (Alive (HaskellElemental x)))
    | f x > 0 = E (Alive (HaskellElemental $ f x))
hit _ u
    | hp u == 0 = u
hit _ _ = E Dead
--5
haskellBlast :: Army -> Army
haskellBlast l = blastMax l $ dmgList l
    where
        blast :: Army -> Army
        blast = map $ hit $ (+) (-5)

        blastMax :: Army -> [Health] -> Army
        blastMax [] _ = []
        blastMax l [] = blast l
        blastMax l@(x:xs) (y:ys)
            | y == maximum (y:ys) = blast (take 5 l) ++ drop 5 l
            | otherwise = x : blastMax xs ys

        dmgList :: Army -> [Health]
        dmgList l@(x:xs)
            | length l < 5 = []
            | otherwise = hpSum (take 5 l) - hpSum (blast $ take 5 l) : dmgList xs
            where
                hpSum :: Army -> Health
                hpSum = sum . map hp
--6
multiHeal :: Health -> Army -> Army
multiHeal h l = heal h (countAlive h l) l
    where
        countAlive :: Health -> Army -> Integer
        countAlive _ [] = 0
        countAlive h (x:xs)
            | h < 1 = 0
            | hp x == 0 = countAlive h xs
            | otherwise = 1 + countAlive (h - 1) xs

        heal :: Health -> Integer -> Army -> Army
        heal _ _ [] = []
        heal h n (x:xs)
            | n < 1 = (x:xs)
            | hp x == 0 = x : heal h n xs
            | otherwise = hit (+ lol) x : heal (h - lol) (n - 1) xs
            where
                lol :: Health
                lol = ceiling $ fromIntegral h / fromIntegral n
--7
battle :: Army -> EnemyArmy -> Maybe Army {- vagy EnemyArmy lesz az eredmény. -}
battle xs ys
    | over xs && over ys = Nothing
    | over xs = Just ys
    | over ys = Just xs
    | otherwise = battle (formationFix $ multiHeal 20 $ haskellBlast $ fight ys xs) (formationFix $ fight xs ys)
--8
chain :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy)
chain _ ([], ys) = ([], ys)
chain n (x:xs, ys)
    | n < 1 = (x:xs, ys)
    | hp x == 0 = mapFst (x:) $ link n (xs, ys)
    | otherwise = mapFst (hit (+ n) x:) $ link (n - 1) (xs, ys)
    where
        mapFst :: (a -> b) -> (a, c) -> (b, c)
        mapFst f (x, y) = (f x, y)

        link :: Amount -> (Army, EnemyArmy) -> (Army, EnemyArmy)
        link _ (xs, []) = (xs, [])
        link n (xs, y:ys)
            | n < 1 = (xs, y:ys)
            | hp y == 0 = mapSnd (y:) $ chain n (xs, ys)
            | otherwise = mapSnd (hit (+ (-n)) y:) $ chain (n - 1) (xs, ys)
            where
                mapSnd :: (a -> b) -> (c, a) -> (c, b)
                mapSnd f (x, y) = (x, f y)
--9
battleWithChain :: Army -> EnemyArmy -> Maybe Army {- vagy Maybe EnemyArmy -}
battleWithChain xs ys
    | over xs && over ys = Nothing
    | over xs = Just ys
    | over ys = Just xs
    | otherwise = battleWithChain (formationFix $ fst t) (formationFix $ snd t)
    where
        t :: (Army, EnemyArmy)
        t = chain 5 (multiHeal 20 $ haskellBlast $ fight ys xs, fight xs ys)
--10
data OneVOne = Winner String | You Health OneVOne | HaskellMage Health OneVOne deriving Eq

instance Show OneVOne where
    show x = '<' : shshowow x
        where
            shshowow :: OneVOne -> String
            shshowow (Winner x) = "|| Winner " ++ x ++ " ||>"
            shshowow (You x y) = "You " ++ show x ++ "; " ++ shshowow y
            shshowow (HaskellMage x y) = "HaskellMage " ++ show x ++ "; " ++ shshowow y
--11
finalBattle :: Health -> Health -> OneVOne
finalBattle x y
    | y < 1 = HaskellMage 0 $ Winner "You"
    | x < 1 = You 0 $ Winner "HaskellMage"
    | y < 4 = HaskellMage y $ youMove (div x 2) $ y * 4
    | x > 20 = HaskellMage y $ youMove (div (x * 3) 4) y
    | otherwise = HaskellMage y $ youMove (x - 11) y
    where
        youMove :: Health -> Health -> OneVOne
        youMove x y
            | x < 1 = You 0 $ Winner "HaskellMage"
            | x < 4 = You x $ finalBattle (x * 4) y
            | y > 15 = You x $ finalBattle x $ div (y * 3) 5
            | otherwise = You x $ finalBattle x $ y - 9
--I am become haskell mage, the destroyer of haskell mage
