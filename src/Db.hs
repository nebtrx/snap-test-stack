{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Db where

import           Control.Applicative
import           Data.ByteString       (ByteString)
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
  , winner     :: Player
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

rowToPlayer :: [SqlValue] -> Player
rowToPlayer (id:name:race:rating:_) =
  Player  { playerID = Just $ fromSql id
          , name = fromSql name
          , race = toEnum (fromSql race :: Int)
          , rating = fromSql rating
          }


rowToGame :: [SqlValue] -> Game
rowToGame row = let
  (rowToPlayer -> p1, row') = splitAt 4 row
  (rowToPlayer -> p2, row'') = splitAt 4 row'
  (id:_:_:outcome:time:url:_) = row
    in Game { gameID = Just $ fromSql id
            , player1 = p1
            , player2 = p2
            , winner = if fromSql outcome == (1::Int) then p1 else p2
            , timePlayed = fromSql time
            , videoUrl = fromSql url
            }

allPosts :: Connection -> IO [Post]
allPosts conn = do
  let query = "SELECT id, title, body, slug, timeposted" `mappend`
              " FROM post ORDER BY timeposted DESC"
  fmap rowToPost <$> quickQuery' conn query []

playerByName :: Connection -> Text -> IO (Maybe Player)
playerByName conn name = do
  let query = "Select id, name, race, rating" `mappend`
              "FROM player WHERE lower(player.name)=?"
  listToMaybe . fmap rowToPlayer <$> quickQuery' conn query [toSql . T.toLower $ name]

allPlayers :: Connection -> IO [Player]
allPlayers conn = do
  let query = "Select id, name, race, rating FROM player" `mappend`
              "ORDER BY rating DESC"
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


games :: Connection -> IO [Game]
games conn = do
  let query = "SELECT * FROM player as player1, player as player2, game" `mappend`
              "WHERE game.player1 = player1.id and game.player2 = player2.id"
  fmap rowToGame <$> quickQuery' conn query []

game :: Connection -> Integer -> IO (Maybe Game)
game conn gameID = do
  let query = "SELECT * FROM player as player1, player as player2, game" `mappend`
              "WHERE game.player1 = player1.id and game.player2 = player2.id" `mappend`
              "AND game.id=?"
  listToMaybe . fmap rowToGame <$> quickQuery' conn query [toSql gameID]

gamesByPlayer :: Connection -> Player -> IO [Game]
gamesByPlayer conn player = do
  let query = "SELECT * FROM player as player1, player as player2, game" ++
              "WHERE game.player1 = player1.id and game.player2 = player2.id" ++
              "AND (player1.id=? or player2.id=?)"
  case playerID player of
        Nothing -> return []
        (Just theid) -> map rowToGame <$> quickQuery' conn query [toSql theid, toSql theid]
