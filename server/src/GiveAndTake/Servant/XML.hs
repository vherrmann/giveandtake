module GiveAndTake.Servant.XML where

import Data.XML.Types (Document (..), Prologue (Prologue))
import GiveAndTake.Prelude
import Network.HTTP.Media qualified as M
import Servant qualified as S
import Text.Feed.Export qualified as Feed
import Text.Feed.Types (Feed)
import Text.XML.Unresolved qualified as XML

data XML

instance S.Accept XML where
  contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance S.MimeRender XML Feed where
  mimeRender _ feed =
    XML.renderLBS
      XML.def
      Document
        { documentPrologue = Prologue [] Nothing []
        , documentRoot = Feed.xmlFeed feed
        , documentEpilogue = []
        }
