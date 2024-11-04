module GiveAndTake.Handlers.Feed where

import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time qualified as C
import Data.Time.Format.ISO8601 qualified as C
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.DB.Types qualified as DB
import GiveAndTake.Handlers.Utils
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Servant (ServerError (..))
import Servant.Server (err401)
import Text.Atom.Feed qualified as Atom
import Text.Feed.Types (Feed (..))
import Text.Feed.Types qualified as F
import Text.RSS.Syntax (RSS (..), RSSChannel (..), RSSItem (..))
import Text.RSS.Syntax qualified as RSS

postToAtomEntry :: UConfig -> P.Entity DB.Post -> User -> Atom.Entry
postToAtomEntry uconfig postEnt user =
  let post = postEnt.val
      url = authUrl uconfig ["post", show postEnt.key]
      date = T.pack $ C.iso8601Show post.createdAt
   in -- FIXME: add source
      ( Atom.nullEntry
          url -- The ID field. Must be a link to validate.
          (Atom.TextString post.title) -- Title
          date
      )
        { Atom.entryAuthors = [Atom.nullPerson{Atom.personName = user.name}]
        , Atom.entryLinks = [Atom.nullLink url]
        , Atom.entryContent = Nothing
        }

fUrl :: UConfig -> UserId -> Text -> Text
fUrl uconfig userId token = authUrl uconfig ["api", "feed", show userId, token]

generateAtomFeed :: (HasHandler m) => WithKey' User -> Text -> [P.Entity DB.Post] -> m Atom.Feed
generateAtomFeed user token posts = do
  uconfig :: UConfig <- askM
  ct <- getUTCTime
  let feedId = fUrl uconfig user.key token
      feedTitle = Atom.TextString [fmt|{user.value.name}'s Feed|]
      feedUpdated = T.pack $ C.iso8601Show ct
      userFeed = Atom.nullFeed feedId feedTitle feedUpdated
  feedEntries <-
    catMaybes <$> for posts \postEnt -> do
      mUser <- runDB $ P.get postEnt.val.user
      when (isNothing mUser) do
        logWarn [fmt|User not found for post {show @Text postEnt}|]
      pure $ postToAtomEntry uconfig postEnt <$> mUser
  pure $
    userFeed
      { Atom.feedEntries = feedEntries
      , Atom.feedLinks = [Atom.nullLink $ authUrl uconfig []]
      }

toRSSDate :: UTCTime -> T.Text
toRSSDate = T.pack . C.formatTime C.defaultTimeLocale C.rfc822DateFormat

postToRSSItem :: UConfig -> P.Entity DB.Post -> User -> RSSItem
postToRSSItem uconfig postEnt user =
  let post = postEnt.val
      url = authUrl uconfig ["post", show postEnt.key]
      date = toRSSDate post.createdAt
   in (RSS.nullItem post.title)
        { rssItemLink = Just url
        , rssItemAuthor = Just user.name
        , rssItemPubDate = Just date
        }

generateRSSFeed :: (HasHandler m) => WithKey' User -> Text -> [P.Entity DB.Post] -> m RSS
generateRSSFeed user token posts = do
  uconfig :: UConfig <- askM
  ct <- getUTCTime
  let feedUrl = fUrl uconfig user.key token
      feedTitle = [fmt|{user.value.name}'s Feed|]
      feedUpdated = toRSSDate ct
  feedItems <-
    catMaybes <$> for posts \postEnt -> do
      mUser <- runDB $ P.get postEnt.val.user
      when (isNothing mUser) do
        logWarn [fmt|User not found for post {show @Text postEnt}|]
      pure $ postToRSSItem uconfig postEnt <$> mUser
  pure $
    RSS
      { rssVersion = "2.0"
      , rssAttrs = []
      , rssChannel =
          (RSS.nullChannel feedTitle feedUrl)
            { rssLastUpdate = Just feedUpdated
            , rssPubDate = Just feedUpdated
            , rssItems = feedItems
            }
      , rssOther = []
      }

getFeedH :: (HasHandler m) => Maybe Text -> UserId -> Text -> m F.Feed
getFeedH accHeader userId token = do
  uconfig <- askM @UConfig
  when (maybe False ("text/html" `T.isInfixOf`) accHeader) do
    redirect303 $ docsUrl uconfig ["feed"]
  logInfo $ show accHeader
  user <- getByKeySE @User userId
  feedEntity <-
    runDB (P.getBy $ UniqueFeedToken token)
      >>= maybeToMErr err401{errBody = "Invalid token"}
  let feed = feedEntity.entityVal
  unless (feed.user == userId) do
    logInfo [fmt|Suspicious activity {userId} {token}|]
    throwError err401{errBody = "Invalid token"}
  case feed.fType of
    MainFeed -> do
      posts <- getFeedPosts userId
      rssFeed <- generateRSSFeed (WithKey userId user) token posts
      pure $ RSSFeed rssFeed

getFeedUrlH :: (HasHandler m) => P.Entity User -> FeedType -> m FeedUrlPostResponse
getFeedUrlH userEntity fType = do
  uconfig :: UConfig <- askM
  let userId = userEntity.key
  mFeedEntity <- runDB (P.getBy $ UniqueFeedKind userId fType)
  feed <- case mFeedEntity of
    Just feedEntity -> pure feedEntity.entityVal
    Nothing -> do
      -- FIXME: secure enough?
      token <- randomUrlToken
      ct <- getUTCTime
      let feed = Feed{user = userId, token = token, fType, createdAt = ct}
      runDB $ insertUUID feed
      pure feed

  pure FeedUrlPostResponse{feedUrl = fUrl uconfig userId feed.token}
