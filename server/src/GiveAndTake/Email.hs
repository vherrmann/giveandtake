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
  deriving stock (Show, Generic)

sendMail :: (HasUConfig m, IsMailAddress a, MonadIO m) => Mail a -> m ()
sendMail mail = do
  uconfig <- askM @UConfig
  dynuconfig <- askM @DynUConfig
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
    mailMethod = case emailConfig.smtpMethod of
      SmtpStartTLS -> MS.sendMailWithLoginSTARTTLS'
      SmtpSSL -> MS.sendMailWithLoginTLS'
      SmtpPlain -> MS.sendMailWithLogin'
  liftIO $
    -- FIXME: better error messages on wrong user credentials
    mailMethod
      (T.unpack emailConfig.smtpHost)
      (fromInteger emailConfig.smtpPort)
      (T.unpack emailConfig.smtpUser)
      (T.unpack dynuconfig.smtpPass)
      newMail
