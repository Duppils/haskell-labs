import Data.List

main = putStrLn "Hello Daniel"

similarityScore :: String -> String -> Int
similarityScore xs ys = simLen (length xs) (length ys)
  where
    simLen i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry :: Int -> Int -> Int
    simEntry i 0 = i * scoreSpace
    simEntry 0 j = j * scoreSpace
    simEntry i j
      | x == y    = scoreMatch + simLen (i-1) (j-1)
      | otherwise = maximum [scoreSpace + simLen i (j-1),
                              scoreSpace + simLen (i-1) j,
                              scoreMismatch + simLen (i-1) (j-1)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b =>  (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\x -> valueFcn (last sortedList) == valueFcn x) sortedList
  where sortedList = sortOn valueFcn xs

type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = optAlign (length xs) (length ys)
  where
    optAlign i j = alignTable!!i!!j
    alignTable = [[ alignEntry i j | j<-[0..]] | i<-[0..] ]

    alignEntry :: Int -> Int -> [AlignmentType]
    alignEntry i 0 = [(drop ((length xs) - i) xs, replicate i '-')]
    alignEntry 0 j = [(replicate j '-', drop ((length ys) - j) ys)]
    alignEntry i j
      | x == y  = attachHeads x y $ optAlign (i-1) (j-1)
      | otherwise = maximaBy currentSimilarity ((attachHeads '-' y (optAlign i (j-1)))
                              ++ (attachHeads x '-' (optAlign (i-1) j))
                              ++ (attachHeads x y (optAlign (i-1) (j-1))))
      where
        x = xs!!(length xs - i)
        y = ys!!(length ys - j)

currentSimilarity :: AlignmentType -> Int
currentSimilarity ([],[]) = 0
currentSimilarity ((x:xs),(y:ys))
  | match = scoreMatch + currentSimilarity (xs, ys)
  | space = scoreSpace + currentSimilarity (xs, ys)
  | otherwise = scoreMismatch + currentSimilarity (xs, ys)
  where match = x == y
        space = x == '-' || y == '-'
