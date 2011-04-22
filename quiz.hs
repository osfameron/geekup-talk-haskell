import System.Random
import Control.Applicative
import Control.Monad
import Data.List (any)
import Data.NumInstances -- to allow (1,1) + (2,2)

data ModuleNode = Quiz String [ModuleNode]
                | Section       String Int     [ModuleNode]
                | RandomSection String Int Int [ModuleNode]
                | Question      String Int      Answer
    deriving Show

data Answer = MultiChoice  [BoolAnswer]
            | StringChoice [String]
    deriving Show

data BoolAnswer = Y String
                | N String
    deriving Show

quiz = Quiz "General Knowledge Quiz" [ pop, geo ]

pop = Section "Pop music" 60 [
    Question "Which of these are Beatles?" 5
        $ MultiChoice [
            Y "John",
            Y "Paul",
            Y "George",
            Y "Ringo",
            N "Bob",
            N "Jason" ],
    Question "Which of these are Sugababes?" 5
        $ MultiChoice [
            Y "Heidi",
            Y "Amelle",
            Y "Jade",
            N "Tracy",
            N "Shirley" ]
        ]

geo = RandomSection "Geography" 40 2 [
    Question "What is the capital of England?" 2 $ StringChoice ["London"],
    Question "What is the capital of France?"  2 $ StringChoice ["Paris"],
    Question "What is the capital of Finland?" 2 $ StringChoice ["Helsinki"],
    Question "What is the capital of Germany?" 2 $ StringChoice ["Berlin"],
    Question "What is the capital of Italy?"   2 $ StringChoice ["Rome", "Roma"]
    ]

data CompletedNode = CompletedNode String (Int,Int) [CompletedNode] ModuleNode
    deriving Show

completeScore (CompletedNode _ i _ _) = i

takeQuiz node@(Question s i a) = do
    putStr s
    putStr " "
    ans <- getLine
    let score = if checkAnswer ans a then (i,i) else (0,i)
    return $ CompletedNode ans score [] node

takeQuiz node@(Section s i ns) = do
    cs <- mapM takeQuiz ns
    let (score, total) = sum $ map completeScore cs
    let score' = (i * score) `div` total
    return $ CompletedNode s (score',i) cs node

checkAnswer s (StringChoice ss) = any (==s) ss

checkAnswer s (MultiChoice ss) = True -- stub for now
            

RandomSection _ _ _ (q:_) = geo

-- "stamps" a variant of the quiz, ready to be allocated to a quiz-taker.

stamp :: ModuleNode -> IO ModuleNode
stamp (Quiz s ns)              = Quiz    s   <$> mapM stamp ns
stamp (Section s i ns)         = Section s i <$> mapM stamp ns
stamp q@(Question _ _ _)       = return q
stamp (RandomSection s i r ns) = do selected <- pickN r ns 
                                    Section s i <$> mapM stamp selected

pickN :: Int -> [a] -> IO [a]
pickN n xs = let len = length xs 
             in  pickN' n len xs

pickN' :: Int -> Int -> [a] -> IO [a] 
pickN' n l []     = do return [] 
pickN' n l (x:xs) = do b <- roll n l 
                       if b then do xs <- pickN' (n-1) (l-1) xs 
                                    return (x:xs)
                       else pickN' n (l-1) xs

roll p q = do r <- getStdRandom (randomR (1,q)) 
              return $ r <= p

{- Twey pasted
    flatten (RandomSection s _ r ns) = do
        rs <- pickN r ns
        rs' <- sequence $ map flatten rs
        return $ Section s r rs

    -- Simplify sequence/map
    flatten (RandomSection s _ r ns) = do
        rs <- pickN r ns
        rs' <- mapM flatten rs
        return $ Section s r rs

    -- Remove unnecessary variable by combining with <$>
    flatten (RandomSection s _ r ns) = do
        rs <- pickN r ns
        Section s r <$> mapM flatten rs

    -- Convert to ‘chain form’
    flatten (RandomSection s _ r ns) = do
        rs <- pickN r ns
        (Section s r <$>) . mapM flatten $ rs

    -- Remove do notation
    flatten (RandomSection s _ r ns) =
        pickN r ns >>= \ rs -> (Section s r <$>) . mapM flatten $ rs

    -- Remove point
    flatten (RandomSection s _ r ns) =
        pickN r ns >>= (Section s r <$>) . mapM flatten

    -- Convert to liftM to make the type look nicer
    flatten (RandomSection s _ r ns) =
        pickN r ns >>= liftM (Section s r) . mapM flatten
-}
