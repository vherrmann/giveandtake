module GiveAndTake.Handlers where

import Data.ByteString.Lazy.Char8 qualified as BL
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Auth
import GiveAndTake.Handlers.Feed
import GiveAndTake.Handlers.Friends
import GiveAndTake.Handlers.Groups (groupsHandler)
import GiveAndTake.Handlers.Job
import GiveAndTake.Handlers.Media
import GiveAndTake.Handlers.Notif (notifHandler)
import GiveAndTake.Handlers.Posts
import GiveAndTake.Handlers.Users (usersHandler)
import GiveAndTake.Prelude
import GiveAndTake.Types
import Servant (ServerError (..), type (:<|>) (..))
import Servant.Auth.Server qualified as SA
import Servant.Server (err401)

protectApi :: SA.AuthResult (Entity User) -> RServer m ProtectedApi
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protectApi (SA.Authenticated userEnt) =
  postsHandler userEnt
    :<|> usersHandler userEnt
    :<|> mediaHandler userEnt
    :<|> friendsHandler userEnt
    :<|> getFeedUrlH userEnt
    :<|> notifHandler userEnt
    :<|> jobHandler userEnt
    :<|> groupsHandler userEnt
-- Otherwise, we return a 401.
protectApi failure = SA.throwAll err401{errBody = "Cookie authentication failed: " <> BL.pack (show failure)}

unprotectedApi :: SA.CookieSettings -> SA.JWTSettings -> RServer m UnprotectedApi
unprotectedApi cs jwt = authHandler cs jwt :<|> getFeedH

server :: SA.CookieSettings -> SA.JWTSettings -> RServer m Api
server cs jwts = protectApi :<|> unprotectedApi cs jwts
