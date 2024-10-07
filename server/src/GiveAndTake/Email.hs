module GiveAndTake.Email where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GiveAndTake.DB
import GiveAndTake.Prelude
import GiveAndTake.Types
import Network.Mail.Mime qualified as MM
import Network.Mail.SMTP qualified as MS

class IsMailAddress a where
  toMailAdress :: a -> MS.Address

instance IsMailAddress User where
  toMailAdress user = MS.Address (Just user.name) user.email

instance IsMailAddress MS.Address where
  toMailAdress = id

data Mail a = Mail
  { to :: [a] -- IsMailAddress a should be fulfilled
  , subject :: Text
  , plainBody :: Text
  , htmlBody :: Maybe Text
  }
  deriving stock (Show)

sendMail :: (HasHandler m, IsMailAddress a) => Mail a -> m ()
sendMail mail = do
  uconfig :: UConfig <- askM
  let
    emailConfig = uconfig.emailConfig
    from = MS.Address (Just uconfig.serviceName) emailConfig.smtpFrom
    to = toMailAdress <$> mail.to
    cc = []
    bcc = []
    subject = mail.subject
    body = MM.plainPart $ TL.fromStrict mail.plainBody
    mHtml = MM.htmlPart . TL.fromStrict <$> mail.htmlBody
    newMail = MS.simpleMail from to cc bcc subject $ [body] <> maybeToList mHtml
  liftIO $
    -- FIXME: better error messages on wrong user credentials
    MS.sendMailWithLoginTLS'
      (T.unpack emailConfig.smtpHost)
      (fromInteger emailConfig.smtpPort)
      (T.unpack emailConfig.smtpUser)
      (T.unpack emailConfig.smtpPass)
      newMail
