{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Applicative ((<|>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.ByteString.Char8
import Data.Either
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment


data Mode = GET | POST deriving (Eq, Ord, Show)

data Entry = Entry {
    enAddr :: ByteString,
    enMode :: Mode,
    enURL  :: ByteString,
    enSID  :: ByteString,
    enAgent :: ByteString
  }
  deriving (Eq, Ord, Show)

parseEntry :: Parser Entry
parseEntry = do
    addr <- C.pack <$> many1' (satisfy (/= ' '))
    {-
    skipWhile (\c -> c /= ' ')
    anyChar
    skipWhile (\c -> c /= ' ')
    anyChar
    -}
    skipWhile (/= ']')
    mode <- (string "] \"GET " *> pure GET) <|> (string "] \"POST " *> pure POST)
    url <- C.pack <$> many1' (satisfy (\c -> c /= ' ' && c /= '?'))
    pairs <- choice [
        char8 '?' *> parsePairs [],
        return []
      ]
    let sid = fromMaybe "" $ lookup "sid" pairs
        url' = url <> encodePairs (filter (\(key, _) -> key /= "sid") pairs)
        
    skipWhile (/= '"')
    char8 '"'
    skipWhile (/= '"')
    char8 '"'
    skipWhile (/= '"')
    string "\" \""
    agent <- C.pack <$> many1' (satisfy (/= '"'))

    return $ Entry {
        enAddr = addr,
        enMode = mode,
        enURL = url',
        enSID = sid,
        enAgent = agent
      }
  where
    parseSid = do
        skipWhile (\c -> c /= '&' && c /= '?')
        choice [
            do
                anyChar
                string "sid="
                C.pack <$> many1' (satisfy (\c -> c /= '&' && c /= ' ')),
            endOfInput *> return "",
            anyChar *> parseSid
          ]
    parsePairs pairs = do
        key <- C.pack <$> many1' (satisfy (/= '='))
        char8 '='
        value <- C.pack <$> many1 (satisfy (\c -> c /= '&' && c /= ' '))
        let pairs' = ((key, value):pairs)
        choice [
              char8 '&' *> parsePairs pairs',
              return pairs'
          ]
    encodePairs [] = ""
    encodePairs [(key, value)] = "?" <> key <> "=" <> value
    encodePairs ((key, value):pairs) = encodePairs pairs <> "&"  <> key <> "=" <> value

main = do
    [fn] <- getArgs
    ls <- C.lines <$> C.readFile fn
    let ps = rights $ map (parseOnly parseEntry) ls
    --forM_ ps print
        m = M.fromListWith (++) $ map (\e -> (enSID e, [e])) ps
        m' = sortBy (comparing fst) $ map (\(sid, es) -> (length es, reverse es)) $ M.toList m
        noSids = fromJust $ M.lookup "" m
    --forM_ noSids print
    forM_ m' $ \(_, es) -> when (looksValid es) $ do
        let addrs = S.toList . S.fromList $ map enAddr es
            agents = S.fromList $ map enAgent es
        C.putStrLn $ ">>> " <> enSID (head es) <> " " <> C.pack (show (length es)) <> " " <>
            C.pack (show (length addrs)) <> " " <> C.pack (show (S.size agents))
            {-
        forM_ addrs $ \addr -> C.putStrLn $ "  " <> addr
        -}
        forM_ es $ \e -> do
            C.putStrLn $ "  " <> enAddr e <> " " {- <> enAgent e <> " " -} <> C.pack (show $ enMode e) <> " " <> enURL e

looksValid :: [Entry] -> Bool
looksValid es =
    S.size agents == 1
  where
    agents = S.fromList $ map enAgent es
