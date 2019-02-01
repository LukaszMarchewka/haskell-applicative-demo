module ValidationDemo
(
  validateNickname,
  validateAge,
  validateUser,
  validateNicknameA,
  validateAgeA,
  validateUserA
)where

import Data.List
import Data.Validation
import Data.List.NonEmpty

type Nickname = String
type Age = Int
data User = User Nickname Age deriving (Show)

--Either

validateNickname :: String -> Either [String] Nickname
validateNickname str
    | size < 1 = Left [ "Nickname must have at least 1 character" ]
    | size > 10 = Left [ "Nickname must have at most 10 characters" ]
    | otherwise = Right str
    where size = Data.List.length str

validateAge :: Int -> Either [String] Age
validateAge val
    | val < 0 = Left [ "Minimal age is 0" ]
    | val > 150 = Left [ "Maximal age is 150" ]
    | otherwise = Right val

validateUser :: String -> Int -> Either [String] User
validateUser nickname age =
    User
    <$> (validateNickname nickname)
    <*> (validateAge age)



--Validation & NonEmpty

validateNicknameA :: String -> Validation (NonEmpty String) Nickname
validateNicknameA str
    | size < 1 = Failure $ "Nickname must have at least 1 character" :| []
    | size > 10 = Failure $ "Nickname must have at most 10 characters" :| []
    | otherwise = Success str
    where size = Data.List.length str

validateAgeA :: Int -> Validation (NonEmpty String) Age
validateAgeA val
    | val < 0 = Failure $ "Minimal age is 0" :| []
    | val > 150 = Failure $  "Maximal age is 150" :| []
    | otherwise = Success val

validateUserA :: String -> Int -> Validation (NonEmpty String) User
validateUserA nickname age =
    User
    <$> (validateNicknameA nickname)
    <*> (validateAgeA age)



--validateUser "john" 20
--Success (User "john" 20)

--validateUser "" 200
--Failure ["Nickname must have at least 1 character","Maximal age is 150"]


--validateUserA "john" 20
--Success (User "john" 20)

--validateUserA "" 200
--Failure ("Nickname must have at least 1 character" :| ["Maximal age is 150"])