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
import User

--Either

validateNickname :: String -> Either [String] Nickname
validateNickname str
    | size < 1 = Left [ "Nickname must have at least 1 character" ]
    | size > 10 = Left [ "Nickname must have at most 10 characters" ]
    | otherwise = Right $ Nickname str
    where size = Data.List.length str

validateAge :: Int -> Either [String] Age
validateAge val
    | val < 0 = Left [ "Minimal age is 0" ]
    | val > 150 = Left [ "Maximal age is 150" ]
    | otherwise = Right $ Age val

validateUser :: String -> Int -> Either [String] User
validateUser nickname age =
    User
    <$> (validateNickname nickname)
    <*> (validateAge age)

--validateUser "john" 20
--Right (User (Nickname {getNickname = "john"}) (Age {getAge = 20}))

--validateUser "" 200
--Left ["Nickname must have at least 1 character"]




--Validation & NonEmpty

validateNicknameA :: String -> Validation (NonEmpty String) Nickname
validateNicknameA str
    | size < 1 = Failure $ "Nickname must have at least 1 character" :| []
    | size > 10 = Failure $ "Nickname must have at most 10 characters" :| []
    | otherwise = Success $ Nickname str
    where size = Data.List.length str

validateAgeA :: Int -> Validation (NonEmpty String) Age
validateAgeA val
    | val < 0 = Failure $ "Minimal age is 0" :| []
    | val > 150 = Failure $  "Maximal age is 150" :| []
    | otherwise = Success $ Age val

validateUserA :: String -> Int -> Validation (NonEmpty String) User
validateUserA nickname age =
    User
    <$> (validateNicknameA nickname)
    <*> (validateAgeA age)

--validateUserA "john" 20
--Success (User (Nickname {getNickname = "john"}) (Age {getAge = 20}))

--validateUserA "" 200
--Failure ("Nickname must have at least 1 character" :| ["Maximal age is 150"])