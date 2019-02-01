module SimpleDemo
(
  createUser,
  readNickname,
  readAge,
  readUserM,
  readUserA,
  readUserA'
) where

import User

createUser :: Nickname -> Age -> User
createUser = User

--createUser (Nickname "john") (Age 20)
--User (Nickname {getNickname = "john"}) (Age {getAge = 20})

readNickname :: IO Nickname
readNickname = do
    _ <- putStrLn "Your nickname:"
    fmap Nickname getLine

readAge :: IO Age
readAge = do
    _ <- putStrLn "Your age:"
    fmap (Age . read) getLine

readUserM :: IO User
readUserM = do
    nickname <- readNickname
    age <- readAge
    return $ User nickname age

readUserA :: IO User
readUserA = pure User <*> readNickname <*> readAge

readUserA' :: IO User
readUserA' = User <$> readNickname <*> readAge

