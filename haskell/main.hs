import Data.Bits
import Data.IORef
import Data.List
import System.IO.Unsafe

-- RNG

theSeed :: IORef Int
theSeed = unsafePerformIO (newIORef 0)

setSeed :: Int -> IO ()
setSeed seed = writeIORef theSeed seed

getSeed :: IO Int
getSeed = readIORef theSeed

randomIO :: IO Int
randomIO = atomicModifyIORef theSeed (swap . random)
    where
        swap (l, r) = (r, l)

random :: Int -> (Int, Int)
random seed = (value, seed')
    where
        seed' = (1103515245 * seed + 12345) .&. 0xFFFFFFFF
        value = (shiftR seed' 16) .&. 0x7FFF

randoms :: Int -> [Int]
randoms seed = value:randoms seed'
    where
        (value, seed') = random seed

-- Collatz number

nextCollatz :: Integer -> Integer
nextCollatz n = if n .&.1 == 0
                    then shiftR n 1
                    else n + 1 + shiftL n 1

calcScore :: Integer -> Integer
calcScore n = calcScoreHelper n 0
  where
    calcScoreHelper 1 score = score
    calcScoreHelper n score = calcScoreHelper (nextCollatz n) (score + 1)

-- Block

data Orientation = West | North | East | South deriving Show

data Operation = L | R | D | C deriving Show

data Block = Block
    { number :: Int
    , orientation :: Orientation
    , operations :: [Operation]
    , finish :: Bool
    } deriving Show

instance Eq Block where
    (Block n1 _ _ _) == (Block n2 _ _ _) = n1 == n2

instance Ord Block where
    (Block n1 _ _ _) `compare` (Block n2 _ _ _) = n1 `compare` n2

type Blocks = [Block]

genBlocks :: [Int] -> [Block]
genBlocks xs = map defaultBlock xs
    where defaultBlock x = (Block x West [] False)

rotate :: Block -> Block
rotate (Block n West ops False) = Block n North (C:ops) False
rotate (Block n North ops False) = Block n East (C:ops) False
rotate (Block n East ops False) = Block n South (C:ops) False
rotate (Block n South ops False) = Block n West (C:ops) False
rotate b@(Block _ _ _ True) = b

finalize :: Block -> Block
finalize (Block n o p _) = Block n o p False

fstCell :: Block -> Int
fstCell (Block n West _ _) = n `div` 100
fstCell (Block n North _ _) = n `div` 100
fstCell (Block n East _ _) = n `mod` 10
fstCell (Block n South _ _) = n `mod` 10

sndCell :: Block -> Int
sndCell (Block n _ _ _) = n `div` 10 `mod` 10

thdCell :: Block -> Int
thdCell (Block n West _ _) = n `mod` 10
thdCell (Block n North _ _) = n `mod` 10
thdCell (Block n East _ _) = n `div` 100
thdCell (Block n South _ _) = n `div` 100

sumBlocks :: [Block] -> Int
sumBlocks bs = foldbs helper bs 0
  where
    foldbs f [] init = init
    foldbs f (b:bs) init = foldbs f bs (f b init)
    helper (Block n _ _ _) init = n + init

isOdd :: [Block] -> Bool
isOdd bs = ((number b) .&. 1) == 1
  where
    b = head $ reverse bs

-- Game

initializeGame :: Int -> (Int, Int, Int, Int, Int)
initializeGame seed =
    let
        (t, seed') = random seed
        (n, seed'') = random seed'
        (h, seed''') = random seed''
        (w, seed'''') = random seed'''
    in
        (t `mod` 196 + 5, n `mod` 1901 + 100,
         h `mod` 81 + 20, w `mod` 181 + 20, seed'''')

play :: [Block] -> Int -> Int -> [Operation]
play bs height width = concatBlockOp $ solve bs height width

concatBlockOp :: [Block] -> [Operation]
concatBlockOp [] = []
concatBlockOp (b:bs) = (operations b) ++ (concatBlockOp bs)

solve :: [Block] -> Int -> Int -> [Block]
solve bs height width
    | xs == [] = bs
    | otherwise = solve bs' height width
  where
    xs = takeFreeBlocks bs width
    bs' = map findRoute $ sortBlocks xs width

takeFreeBlocks :: [Block] -> Int -> [Block]
takeFreeBlocks [] _ = []
takeFreeBlocks _ 0 = []
takeFreeBlocks (b@(Block _ _ _ False):bs) n = b:takeFreeBlocks bs (n - 1)
takeFreeBlocks (_:bs) n = takeFreeBlocks bs n

exchange :: [Block] -> Int -> Int -> [Block]
exchange bs i j = (take i bs) ++ (bs !! i : []) ++ (drop (i + 1) (take j bs)) ++ (bs !! j : []) ++ (drop (j + 1) bs)

sortBlocks3 :: [Block] -> [Block]
sortBlocks3 bs = helper 1 bs'
  where
    bs' = reverse $ sort $ map Main.rotate bs
    helper n xs = if isOdd xs
                    then xs
                    else helper (n + 1) $ reverse $ exchange (reverse xs) 0 n

sortBlocks2 :: [Block] -> [Block]
sortBlocks2 bs = bs

-- Sorts the given blocks in order to result in a good score
sortBlocks :: [Block] -> Int -> [Block]
sortBlocks bs width = if (sumBlocks bs) `mod` 3 == 0
                    then sortBlocks3 bs
                    else sortBlocks2 bs

pop :: Integer -> Int
pop x = pop' x 0
  where
    pop' 0 n = n
    pop' x n = pop' (x .&. (x - 1)) (n + 1)

findRoute :: Block -> Block
findRoute b = b

main = do
    let seed = 10
    let (timeout, nblocks, height, width, seed') = initializeGame seed
    putStrLn ("timeout " ++ (show timeout))
    putStrLn ("nblocks " ++ (show nblocks))
    putStrLn ("height " ++ (show height))
    putStrLn ("width " ++ (show width))
    let bs = map (`mod` 1000) $ take nblocks $ randoms seed'
    let blocks = genBlocks bs
    print $ play blocks height width

