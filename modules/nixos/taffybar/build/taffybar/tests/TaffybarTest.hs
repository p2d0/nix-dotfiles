module TaffybarTest where
import Test.HUnit
import Network.HTTP.Client

  ( HttpException,
    Request (requestHeaders),
    Response (responseBody, responseStatus),
    parseRequest_
  )

tests = TestList [
  TestCase (assertEqual "Not equal" 1 1),
  TestCase  (assertEqual "" parseRequest_ "pepega")
  ]
