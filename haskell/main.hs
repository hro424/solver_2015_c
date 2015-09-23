import Data.Bits
import Data.IORef
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

calcScore :: Integer -> Integer
calcScore n = calcScoreHelper n 0
    where
        calcScoreHelper 1 score = score
        calcScoreHelper n score = calcScoreHelper nn (score + 1)
            where
                nn = if n .&. 1 == 0 then shiftR n 1
                                     else n + 1 + shiftL n 1

-- Block

data Orientation = West | North | East | South

data Block = Block { number :: Int, orientation :: Orientation }

--genBlock :: IO Block
--genBlock = do
--    n <- random `mod` 1000
--    IO Block n West

rotate :: Block -> Block
rotate (Block n West) = Block n North
rotate (Block n North) = Block n East
rotate (Block n East) = Block n South
rotate (Block n South) = Block n West

fstBlock :: Block -> Int
fstBlock (Block n West) = n `div` 100
fstBlock (Block n North) = n `div` 100
fstBlock (Block n East) = n `mod` 10
fstBlock (Block n South) = n `mod` 10

sndBlock :: Block -> Int
sndBlock (Block n _) = n `div` 10 `mod` 10

thdBlock :: Block -> Int
thdBlock (Block n West) = n `mod` 10
thdBlock (Block n North) = n `mod` 10
thdBlock (Block n East) = n `div` 100
thdBlock (Block n South) = n `div` 100

main = do
    setSeed 10
    t <- random
    let timeout = t `mod` 196 + 5
    n <- random
    let nblocks = n `mod` 1901 + 100
    h <- random
    let height = h `mod` 81 + 20
    w <- random
    let width = w `mod` 181 + 20
    putStrLn ("timeout " ++ (show timeout))
    putStrLn ("nblocks " ++ (show nblocks))
    putStrLn ("height " ++ (show height))
    putStrLn ("width " ++ (show width))
    bs <- take nblocks $ randoms
    let blocks = map (mod 1000) bs
    putStrLn "end"
    
