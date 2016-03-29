{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Control.Applicative
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time.Clock
import           Database.HDBC
import           Database.HDBC.Sqlite3


data Post = Post
  { postID     :: Maybe Integer
  , title      :: Text
  , body       :: Text
  , slug       :: Text
  , timePosted :: UTCTime
  } deriving (Eq, Show, Read)

data Player = Player
  { playerID :: Maybe Integer
  , name     :: Text
  , race     :: Race
  , rating   :: Integer
  } deriving (Eq, Show, Read)


data Race = Terran | Protoss | Zerg
    deriving (Eq, Show, Read, Enum, Ord)

data Game = Game
  { gameID     :: Maybe Integer
  , player1    :: Player
  , player2    :: Player
  , timePlayed :: UTCTime
  , videoUrl   :: ByteString
  } deriving (Eq, Show, Read)

rowToPost :: [SqlValue] -> Post
rowToPost (id:title:body:slug:timeposted:_) =
  Post { postID = Just $ fromSql id
       , title = fromSql title
       , body = fromSql body
       , slug = fromSql slug
       , timePosted = fromSql timeposted
       }

rowToPlayer :: [SqlValue] -> Post
rowToPlayer (id:name:race:rating:_) =
  Player  { playerID = Just $ fromSql id
          , name = fromSql name
          , race = toEnum (fromSql race :: Int)
          , rating = fromSql rating
          }


rowToGame :: [SqlValue] -> Post
rowToGame (id:name:race:rating:_) =
  Player  { playerID = Just $ fromSql id
          , name = fromSql name
          , race = toEnum (fromSql race :: Int)
          , rating = fromSql rating
          }


allPosts :: Connection -> IO [Post]
allPosts conn = do
  let query = "SELECT id, title, body, slug, timeposted" `mappend`
              " FROM post ORDER BY timeposted DESC"
  fmap rowToPost <$> quickQuery' conn query []

playerByName :: Connection -> Text -> IO (Maybe Player)
playerByName conn = do
  let query = "Select id, name, race, rating" `mappend`
              "FROM player WHERE lower(player.name)=?"
  listToMaybe . fmap rowToPlayer <$> quickQuery' conn query [toSql . T.toLower $ name]

allPlayers :: Connection -> IO [Player]
allPlayers conn = do
  let query = "Select id, name, race, rating FROM player"
  fmap rowToPlayer <$> quickQuery' conn query []


putPlayer :: Connection -> Player -> IO ()
putPlayer conn player = do
  let query = "INSERT INTO player(name, race, rating) VALUES(?,?,?)"
  quickQuery' conn query
    [toSql $ name player
    ,toSql $ fromEnum . race player
    ,toSql $ rating player
    ]
  commit conn
  return ()

--
-- games :: Connection -> IO [Game]
-- game :: Connection -> Integer -> IO (Maybe Game)
-- gamesByPlayer :: Connection -> Player -> IO [Game]
