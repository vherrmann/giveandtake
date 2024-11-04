{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

-- import Servant.TypeScript

import Control.Lens
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString.Lazy.Char8 qualified as BL
import GiveAndTake.Api
import GiveAndTake.Prelude
import Servant ((:>))
import Servant qualified as S
import Servant.Multipart qualified as SM
import Servant.OpenApi qualified as SO
import System.Directory qualified as D
import System.Environment (getArgs)
import qualified Text.Feed.Types as F
import Data.OpenApi.Lens
import qualified Data.OpenApi as SO
import Servant.OpenApi (HasOpenApi)
import Servant.Auth qualified as SA
import qualified Data.Text as T
import Data.OpenApi (OpenApi (..), Components (..), ToSchema, ToParamSchema)
import qualified Data.HashMap.Strict.InsOrd as HM
import qualified GiveAndTake.DB.Types as DB
import Data.Coerce
import Data.UUID (UUID)
import GiveAndTake.Types (WithKey)
import qualified Database.Persist as P
import Data.Typeable (Typeable)

--- https://github.com/biocad/servant-openapi3/issues/42

instance (HasOpenApi api) => HasOpenApi (SA.Auth '[] a :> api) where
  toOpenApi Proxy = SO.toOpenApi $ Proxy @api

instance (HasOpenApi (SA.Auth auths a :> api)) => HasOpenApi (SA.Auth (SA.BasicAuth ': auths) a :> api) where
  toOpenApi Proxy = addSecurity $ SO.toOpenApi $ Proxy @(SA.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "BasicAuth"
    securityScheme =
      SO.SecurityScheme
        { _securitySchemeType = SO.SecuritySchemeHttp SO.HttpSchemeBasic
        , _securitySchemeDescription = Just "Basic Authentication"
        }

instance (HasOpenApi (SA.Auth auths a :> api)) => HasOpenApi (SA.Auth (SA.JWT ': auths) a :> api) where
  toOpenApi Proxy = addSecurity $ SO.toOpenApi $ Proxy @(SA.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "JWT"
    securityScheme =
      SO.SecurityScheme
        { _securitySchemeType = SO.SecuritySchemeHttp $ SO.HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Bearer Authentication"
        }

instance (HasOpenApi (SA.Auth auths a :> api)) => HasOpenApi (SA.Auth (SA.Cookie ': auths) a :> api) where
  toOpenApi Proxy = addSecurity $ SO.toOpenApi $ Proxy @(SA.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "Cookie"
    securityScheme =
      SO.SecurityScheme
        { _securitySchemeType = SO.SecuritySchemeHttp $ SO.HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Cookie Authentication"
        }

addSecurityScheme :: T.Text -> SO.SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
  openApi
    { _openApiComponents =
        (_openApiComponents openApi)
          { _componentsSecuritySchemes =
              _componentsSecuritySchemes (_openApiComponents openApi)
                <> SO.SecurityDefinitions (HM.singleton securityIdentifier securityScheme)
          }
    }

addSecurityRequirement :: T.Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
  SO.allOperations
    . security
    %~ ((SO.SecurityRequirement $ HM.singleton securityRequirement []) :)

--- further instances

-- Fix this/replace file upload with Octet-Stream
instance SO.HasOpenApi api =>
         SO.HasOpenApi (SM.MultipartForm t x :> api) where
  toOpenApi _ = SO.toOpenApi (Proxy @api)

instance HasOpenApi S.RawM where
  toOpenApi _ = mempty & paths . at "/" ?~ mempty

--- data type instances
-- The constraint `Coercible (P.Key a) UUID` is important for the correctness of the definition
instance (Typeable a,Coercible (P.Key a) UUID) => ToSchema (P.Key a) where
  declareNamedSchema _ = SO.declareNamedSchema (Proxy @UUID)

-- The constraint `Coercible (P.Key a) UUID` is important for the correctness of the definition
instance (Typeable a,Coercible (P.Key a) UUID) => ToParamSchema (P.Key a) where
  toParamSchema _ = SO.toParamSchema (Proxy @UUID)


data WithUUID b = WithUUID
  { key :: UUID
  , value :: b
  }
  deriving stock (Generic)
instance ToSchema b => ToSchema (WithUUID b)

-- HACK: We don't want too long type names in our typescript client (and also I don't be bothered to change my code to use WithKey$X$Y)
instance (Typeable a, Coercible (P.Key a) UUID, ToSchema b) => ToSchema (WithKey a b) where
  declareNamedSchema _ = SO.declareNamedSchema (Proxy @(WithUUID b))

instance ToSchema DB.Post
instance ToSchema LockedHiddenPostData
instance ToSchema HiddenPostData
instance ToSchema UnhiddenPostData
instance ToSchema ApiPost
instance ToSchema NewPost
instance ToSchema UploadMediaResponse
instance ToSchema LoginData
instance ToSchema DB.User
instance ToSchema SuccessLoginResponse
instance ToSchema SignupData
instance ToSchema VerifyEmail
instance ToSchema UserPublic
instance ToSchema CheckResponse
instance ToSchema FriendsRequestGetResponse
instance ToSchema FeedUrlPostResponse
instance ToSchema DB.Feed
instance ToSchema F.Feed where
    declareNamedSchema _ = do
        -- FIXME: return proper spec
        pure $ SO.NamedSchema (Just "Feed") mempty
instance ToSchema DB.FeedType
instance ToSchema DB.NotifPrio
instance ToSchema DB.NotifWelcomeMsg
instance ToSchema DB.NotifContent
instance ToSchema DB.Notification
instance ToSchema DB.JobStatus
instance ToSchema UnlockedHiddenPostData
instance ToSchema DB.Group
instance ToSchema NewGroup
instance ToSchema GroupPublic
instance ToSchema DB.GroupRole
instance ToSchema ChangeGroupRole
instance ToSchema ApiGroup

main :: IO ()
main =
{- FOURMOLU_DISABLE -}
    let spec = A.encodePretty $ SO.toOpenApi (Proxy @Api)
                    & info . SO.title .~ "GiveAndTake API"
                    & info . version  .~ "0.1"
                    -- & info.description  ?~ "This is an API for the Users service"
                    -- & info.license      ?~ "MIT"
                    -- & host              ?~ "example.com"
{- FOURMOLU_ENABLE -}
     in do
            args <- getArgs
            let file : _ = args
            -- check if file is valid
            existp <- D.doesFileExist file
            if existp
                then putStrLn @Text "File already exists"
                else BL.writeFile file spec

-- rm swagger.json; cabal run giveandtake-generate-typescript -- swagger.json && openapi-generator-cli generate -i swagger.json -g typescript-axios -o ~/repos/giveandtake/client/src/api/autogen
-- watchexec --restart -e hs --watch src/GiveAndTake/Api.hs -- bash -c '"rm swagger.json && cabal run giveandtake-generate-typescript -- swagger.json && openapi-generator-cli generate -i swagger.json -g typescript-fetch -o ~/repos/giveandtake/client/src/api/autogen/"'
