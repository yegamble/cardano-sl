import           Universum

import           Test.Pos.Binary.Tripping (runTests)
import           Test.Pos.Core.TestTxp (tests)

main :: IO ()
main = runTests [tests]
