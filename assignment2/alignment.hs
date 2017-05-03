import Data.List

main = do
  putStrLn "Enter first string:"
  string1 <- getLine
  putStrLn "Enter second string:"
  string2 <- getLine
  outputAlignments string1 string2

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
optAlignments xs ys = snd $ optAlign (length xs) (length ys)
  where
    optAlign i j = alignTable!!i!!j
    alignTable = [[ alignEntry i j | j<-[0..]] | i<-[0..] ]

    alignEntry :: Int -> Int -> (Int, [AlignmentType])
    alignEntry i 0 = (i * scoreSpace, [(drop ((length xs) - i) xs, replicate i '-')])
    alignEntry 0 j = (j * scoreSpace, [(replicate j '-', drop ((length ys) - j) ys)])
    alignEntry i j
      | x == y  = (scoreMatch + fst (optAlign (i-1) (j-1)), attachHeads x y $ snd (optAlign (i-1) (j-1)))
      | otherwise = (fst $ head alignments, foldr ((++) . snd) [] alignments)
        where
          alignments = maximaBy fst
                          [(scoreSpace + fst (optAlign i (j-1)), attachHeads '-' y (snd (optAlign i (j-1))))
                          ,(scoreSpace + fst (optAlign (i-1) j), attachHeads x '-' (snd (optAlign (i-1) j)))
                          ,(scoreMismatch + fst (optAlign (i-1) (j-1)), attachHeads x y (snd (optAlign (i-1) (j-1))))]

          x = xs!!(length xs - i)
          y = ys!!(length ys - j)


outputAlignments :: String -> String -> IO ()
outputAlignments string1 string2 = do
  let alignments = optAlignments string1 string2
  let size = length alignments
  putStrLn $ "There are " ++ show size ++ " optimal alignments:\n"
  mapM (\pair -> putStrLn $ (fst pair) ++ "\n" ++ (snd pair) ++ "\n") alignments
  putStrLn $ "There were " ++ show size ++ " optimal alignments!"
