import Test.HUnit
import qualified Debug.Trace as D

import Network.HTTP.Client
  ( HttpException,
    Request (requestHeaders),
    Response (responseBody, responseStatus),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest, parseRequest_
  )

test1 :: Test
test1 = TestCase (assertEqual "test" 1 1)

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = do
  runTestTT tests
