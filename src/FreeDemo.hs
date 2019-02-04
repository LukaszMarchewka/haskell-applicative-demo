module FreeDemo
(
  user,
  comment,
  parse,
  genDoc,
  printDoc
) where

import Control.Applicative.Free
import Text.Read
import Text.Printf

type Name = String
type Doc = String

data Field a =
    StringField Name Doc
    | IntField Name Doc
    | BoolField Name Doc
    | ObjectField Name Doc (Ap Field a)

type Dsl a = Ap Field a

string :: Name -> Doc -> Dsl String
string n d = liftAp $ StringField n d

int :: Name -> Doc -> Dsl Int
int n d = liftAp $ IntField n d

bool :: Name -> Doc -> Dsl Bool
bool n d = liftAp $ BoolField n d

object :: Name -> Doc -> Dsl a -> Dsl a
object n d a = liftAp $ ObjectField n d a

data User = User String Int Bool deriving Show
data Comment = Comment String User deriving Show

user :: Dsl User
user = User
  <$> string "name" "user's name"
  <*> int "age" "user's age"
  <*> bool "active" "is active"

comment :: Dsl Comment
comment = Comment
  <$> string "title" "short desc"
  <*> object "creator" "who created the comment" user

parse :: Field a -> IO a
parse = undefined

genDoc :: Int -> Field a -> [String]
genDoc indent (StringField n d) = [printf "%*s- %s : string - %s" indent "" n d]
genDoc indent (IntField n d) = [printf "%*s- %s : int - %s" indent "" n d]
genDoc indent (BoolField n d) = [printf "%*s- %s : bool - %s" indent "" n d]
genDoc indent (ObjectField n d m) = asString : nested
  where
    asString = printf "%*s- %s : obj - %s" indent "" n d
    nested = runAp_ (genDoc $ indent + 2) m

printDoc :: IO ()
printDoc = mapM_ putStrLn lines
  where lines = runAp_ (genDoc 0) comment