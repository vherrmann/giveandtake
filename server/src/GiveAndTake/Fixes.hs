{-# OPTIONS_GHC -Wno-orphans #-}

module GiveAndTake.Fixes where

import GiveAndTake.Prelude
import Network.Wai qualified as W
import Servant qualified as S
import Servant.Auth.Server.Internal.AddSetCookie qualified as SI

-- Fix RawM + Auth: https://github.com/haskell-servant/servant/pull/1551
-- Type instance for RawM for SetCookie API
type instance SI.AddSetCookieApi S.RawM = S.RawM
type ApplicationM m = W.Request -> (W.Response -> IO W.ResponseReceived) -> m W.ResponseReceived
instance SI.AddSetCookies ('SI.S n) (S.Tagged m (ApplicationM m)) (S.Tagged m (ApplicationM m)) where
  addSetCookies cookies r = S.Tagged $ \request respond ->
    S.unTagged r request $ respond . W.mapResponseHeaders (<> SI.mkHeaders cookies)
instance
  (Functor m) =>
  SI.AddSetCookies ('SI.S n) (m (ApplicationM m)) (m (ApplicationM m))
  where
  addSetCookies cookies = fmap $ SI.addSetCookies cookies

instance SI.AddSetCookies ('SI.S n) (ApplicationM m) (ApplicationM m) where
  addSetCookies cookies r request respond =
    r request $ respond . W.mapResponseHeaders (<> SI.mkHeaders cookies)
