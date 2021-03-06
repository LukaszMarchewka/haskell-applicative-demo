module Composition
(
  readValidNickname,
  readValidAge,
  readValidUserM,
  readValidUserA
) where

import GHC.Generics
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.Maybe
import User
import Text.Read

readValidNickname :: IO (Maybe Nickname)
readValidNickname = do
    putStrLn "Your name:"
    str <- getLine
    if length str > 0
        then return $ Just $ Nickname str
        else return Nothing

readValidAge :: IO (Maybe Age)
readValidAge = do
    putStrLn "Your age:"
    str <- getLine
    return $ fmap Age $ readMaybe str

--Monad Transformers

readValidUserM :: IO (Maybe User)
readValidUserM = runMaybeT $ do
    nickname <- MaybeT readValidNickname
    age <- MaybeT readValidAge
    return $ User nickname age



--Applicative

readValidUserA :: IO (Maybe User)
readValidUserA = unComp1 (User <$> (Comp1 readValidNickname) <*> (Comp1 readValidAge))
