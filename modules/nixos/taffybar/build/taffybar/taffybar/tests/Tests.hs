{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import System.Taffybar.Util (runCommand)
import Test.HUnit

-- import Network.HTTP.Client
--   ( HttpException,
--     Request (requestHeaders),
--     Response (responseBody, responseStatus),
--     defaultManagerSettings,
--     httpLbs,
--     newManager,
--     parseRequest, parseRequest_
--   )

test1 :: Test
test1 =
  TestCase
    ( do
        r <- createProcess (proc "curl" ["-s", "https://wttr.in/"])
        -- runCommand "curl" ["-s", "https://wttr.in/"] >>= either print (assertEqual "pepega" "")
    )

-- testParsing :: Test
-- testParsing = TestCase $ do
--                            request <-parseRequest_ "pepega"
--                            assertEqual "" request

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = do
  runTestTT tests
