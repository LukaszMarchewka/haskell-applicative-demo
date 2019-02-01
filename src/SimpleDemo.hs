module SimpleDemo
(
  createUser,
  readNickname,
  readAge,
  readUserM,
  readUserA,
  readUserA'
) where

type Nickname = String
type Age = Int
data User = User Nickname Age deriving (Show)

createUser :: Nickname -> Age -> User
createUser = User

--createUser "john" 20
--User "john" 20

readNickname :: IO Nickname
readNickname = do
    _ <- putStrLn "Your nickname:"
    getLine

readAge :: IO Age
readAge = do
    _ <- putStrLn "Your age:"
    fmap read getLine

readUserM :: IO User
readUserM = do
    nickname <- readNickname
    age <- readAge
    return $ User nickname age

readUserA :: IO User
readUserA = pure User <*> readNickname <*> readAge

readUserA' :: IO User
readUserA' = User <$> readNickname <*> readAge

