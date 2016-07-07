import Data.List
import Data.Map (Map, fromListWith, assocs)

data Sample = Sample { descriptor :: [ Double ], label :: String } deriving Show

euclideanDist :: Sample -> Sample -> Double
euclideanDist a b = sqrt $ sum [ ( a - b ) ** 2 | (a, b) <- zip (descriptor a) (descriptor b) ]

nearestNeighbors k trainingSamples query = take k $ sortOn fst $ map distToQuery trainingSamples
                                           where distToQuery s = ( euclideanDist query s, s )
countVotes :: [ Sample ] -> Map String Integer
countVotes neighbors = fromListWith (+) $ [ ( l, 1 ) | l <- map label neighbors ]

highestVoted :: Map String Integer -> String
highestVoted = ( fst . last . sortOn snd . assocs )

trainKnn k trainingSamples = ( highestVoted . countVotes . map snd . nearestNeighbors k trainingSamples )

trainingSamples = [ Sample { descriptor = [1,2], label = "cat" },
                    Sample { descriptor = [0,1], label = "cat" },
                    Sample { descriptor = [3,3], label = "cat" },
                    Sample { descriptor = [100,110], label = "dog" },
                    Sample { descriptor = [101,120], label = "dog" },
                    Sample { descriptor = [201, 200], label = "dog" } ]

mysteriousCreature = Sample { descriptor = [5,5], label = "???" }  -- label = "???" is a temporary hack
knnClassify = trainKnn 4 trainingSamples
guess = knnClassify mysteriousCreature
