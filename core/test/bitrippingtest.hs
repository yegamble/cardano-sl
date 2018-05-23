import           Universum

import           Test.Pos.Binary.Tripping (runTests)

import qualified Test.Pos.Core.TxInWitnessHH (hedgeTests)

main :: IO ()
main = runTests
           [ Test.Pos.Core.TxInWitnessHH.hedgeTests
           ]
