module User
(
  Nickname(..),
  Age(..),
  User(..)
) where

newtype Nickname = Nickname { getNickname :: String } deriving (Show)

newtype Age = Age { getAge :: Int } deriving (Show)

data User = User Nickname Age deriving (Show)
