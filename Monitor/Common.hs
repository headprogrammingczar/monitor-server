module Monitor.Common where

import Data.Attoparsec.Text
import Data.Time
import System.Locale
import Control.Applicative
import qualified Data.Text as T
import Data.Char

data Event
  = Invalid
  | Ping UTCTime String String -- Ping when nick line

parseEvent :: String -> IO Event
parseEvent str = do
  now <- getCurrentTime
  let txt = T.pack str
  let result = parseOnly (parseEvent' now <* endOfInput) txt
  case result of
    Left _ -> return Invalid
    Right x -> return x

parseEvent' :: UTCTime -> Parser Event
parseEvent' now = do
  cmd <- takeTill isSpace
  skipSpace
  case T.unpack cmd of
    "Ping" -> do
      (nick, line) <- parsePing
      return (Ping now nick line)

parsePing = do
  nick <- takeTill isSpace
  skipSpace
  line <- takeText
  return (T.unpack nick, T.unpack line)

prettyPrint :: Event -> String
prettyPrint Invalid = "Invalid"
prettyPrint (Ping when nick line) =
  let when' = formatTime defaultTimeLocale "%T [%a]" when
   in when' ++ " <" ++ nick ++ "> " ++ line

