{-# LANGUAGE UndecidableInstances #-}

module GiveAndTake.Prelude (
  module X,
  headMaybe,
  guardM,
  ifM,
  unlessM,
  whenM,
  undefined,
  error,
  trace,
  traceShow,
  traceShowM,
  traceM,
  traceIO,
  todo,
  leftToMaybe,
  rightToMaybe,
  maybeToRight,
  maybeToLeft,
  maybeToEither,
  print,
  putLText,
  putText,
  putStrLn,
  putStr,
  Type,
  (<<),
  show,
  MonadReaderM (..),
  maybeToMErr,
  MonadLoggerWOCStack,
  MonadLogger,
  whenMLet,
  whenLet,
) where

-- inspired by module from kmonad

import Control.Applicative as X
import Control.Category as X (Category, id, (.))
import Control.Monad as X hiding (return)
import Control.Monad.IO.Class as X
import Data.Bifunctor as X
import Data.Bool as X
import Data.ByteString as X (ByteString)

import Data.ByteString qualified as BS hiding (putStr)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL hiding (putStr)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Char as X (Char)
import Data.Either as X
import Data.Eq as X
import Data.Foldable as X hiding (
  foldl1,
  foldr1,
  maximum,
  maximumBy,
  minimum,
  minimumBy,
 )
import Data.Function as X hiding (id, (.))
import Data.Functor as X (
  ($>),
  (<&>),
 )
import Data.Int as X
import Data.Kind
import Data.List as X (
  break,
  drop,
  filter,
  intercalate,
  isPrefixOf,
  replicate,
  reverse,
  splitAt,
  take,
 )
import Data.List.NonEmpty as X (NonEmpty)
import Data.Type.Equality as X (type (~))

import Control.Monad.Reader as X (MonadReader (..), ReaderT (..))
import Data.Data as X (Proxy (..))
import Data.Default as X (Default (..))
import Data.Maybe (listToMaybe)
import Data.Maybe as X hiding (listToMaybe)
import Data.Monoid as X
import Data.Ord as X
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Traversable as X
import Data.Tuple as X
import Debug.Trace qualified as T
import GHC.Generics as X (Generic)
import PyF as X
import System.IO as X (IO)
import Text.Show as X (Show)

import Prelude as X (Double, Floating (..), Fractional (..), Integer, Num (..), Rational, RealFloat (..), RealFrac (..), fromIntegral)

import Control.Monad.Error.Class qualified as X (throwError)
import Control.Monad.Except (ExceptT, MonadError (..), mapExceptT)
import Control.Monad.Identity (IdentityT, mapIdentityT)
import Control.Monad.Logger.CallStack as X hiding (MonadLogger)
import Control.Monad.Logger.CallStack qualified as L
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Data.Aeson as X (FromJSON, ToJSON)
import GHC.Stack (HasCallStack)
import Prelude qualified as P

default (Text)

{-# WARNING undefined "'undefined' remains in code" #-}
{-# INLINEABLE undefined #-}
undefined :: (HasCallStack) => a
undefined = P.undefined

{-# WARNING error "'error' remains in code" #-}
{-# INLINEABLE error #-}
error :: (HasCallStack) => P.String -> a
error = P.error

{-# WARNING trace "'trace' remains in code" #-}
{-# INLINEABLE trace #-}
trace :: P.String -> a -> a
trace = T.trace

{-# WARNING traceShow "'traceShow' remains in code" #-}
{-# INLINEABLE traceShow #-}
traceShow :: (P.Show a) => a -> a
traceShow a = T.trace (P.show a) a

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
{-# INLINEABLE traceShowM #-}
traceShowM :: (P.Show a, P.Monad m) => a -> m ()
traceShowM a = T.traceM (P.show a)

{-# WARNING traceM "'traceM' remains in code" #-}
{-# INLINEABLE traceM #-}
traceM :: (P.Monad m) => P.String -> m ()
traceM = T.traceM

{-# WARNING traceIO "'traceIO' remains in code" #-}
{-# INLINEABLE traceIO #-}
traceIO :: P.String -> P.IO ()
traceIO = T.traceIO

{-# WARNING todo "'todo' remains in code" #-}
{-# INLINEABLE todo #-}
todo :: (HasCallStack) => a
todo = P.error "Not implemented"

{-# INLINEABLE whenM #-}
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

{-# INLINEABLE unlessM #-}
unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

{-# INLINEABLE whenLet #-}
whenLet :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenLet = for_

{-# INLINEABLE whenMLet #-}
whenMLet :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenMLet p m = traverse_ m =<< p

{-# INLINEABLE ifM #-}
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

{-# INLINEABLE guardM #-}
guardM :: (MonadPlus m) => m Bool -> m ()
guardM f = guard =<< f

-- | A helper function for showing labels
{-# INLINEABLE show #-}
show :: (IsString s, Show a) => a -> s
show = fromString . P.show

{-# INLINEABLE leftToMaybe #-}
leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

{-# INLINEABLE rightToMaybe #-}
rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

{-# INLINEABLE maybeToRight #-}
maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

{-# INLINEABLE maybeToLeft #-}
maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

{-# INLINEABLE maybeToEither #-}
maybeToEither :: (Monoid b) => (a -> b) -> Maybe a -> b
maybeToEither = maybe mempty

{-# INLINEABLE maybeToMErr #-}
maybeToMErr :: (MonadError e m) => e -> Maybe a -> m a
maybeToMErr err = maybe (throwError err) pure

type LText = TL.Text

type LByteString = BL.ByteString

class Print a where
  putStr :: (MonadIO m) => a -> m ()
  putStrLn :: (MonadIO m) => a -> m ()

instance Print T.Text where
  {-# INLINEABLE putStr #-}
  {-# INLINEABLE putStrLn #-}
  putStr = liftIO . T.putStr
  putStrLn = liftIO . T.putStrLn

instance Print LText where
  {-# INLINEABLE putStr #-}
  putStr = liftIO . TL.putStr
  {-# INLINEABLE putStrLn #-}
  putStrLn = liftIO . TL.putStrLn

instance Print BS.ByteString where
  {-# INLINEABLE putStr #-}
  putStr = liftIO . BS.putStr
  {-# INLINEABLE putStrLn #-}
  putStrLn = liftIO . BS.putStrLn

instance Print LByteString where
  {-# INLINEABLE putStr #-}
  putStr = liftIO . BL.putStr
  {-# INLINEABLE putStrLn #-}
  putStrLn = liftIO . BL.putStrLn

instance Print P.String where
  {-# INLINEABLE putStr #-}
  putStr = liftIO . P.putStr
  {-# INLINEABLE putStrLn #-}
  putStrLn = liftIO . P.putStrLn

-- For forcing type inference
{-# INLINEABLE putText #-}
putText :: (MonadIO m) => T.Text -> m ()
putText = putStrLn
{-# SPECIALIZE putText :: T.Text -> IO () #-}

{-# INLINEABLE putLText #-}
putLText :: (MonadIO m) => TL.Text -> m ()
putLText = putStrLn
{-# SPECIALIZE putLText :: TL.Text -> IO () #-}

{-# INLINEABLE headMaybe #-}
headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

{-# INLINEABLE print #-}
print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . P.print

(<<) :: forall (m :: Type -> Type) a b. (Monad m) => m a -> m b -> m a
(<<) = flip (>>)

type MonadLogger m = (HasCallStack, L.MonadLogger m)
type MonadLoggerWOCStack m = (L.MonadLogger m)

--- Hacks
-- We want to be able to have multiple ReaderT in our stack, to be used like
-- test :: (MonadReaderM Int m, MonadReaderM [Int] m, MonadIO m) => m ()
-- test = do
--   x :: Int <- askM
--   l :: [Int] <- askM
--   print (take x l)

-- https://stackoverflow.com/questions/58947160/why-is-functionaldependency-needed-for-defining-monadreader
-- less convenience, but more power
class (Monad m) => MonadReaderM r m where
  askM :: m r
  localM :: (r -> r) -> m a -> m a
  readerM :: (r -> a) -> m a

instance (Monad m) => MonadReaderM r (ReaderT r m) where
  askM = ask
  localM = local
  readerM = reader

instance {-# OVERLAPPABLE #-} (Monad m, MonadReaderM r m) => MonadReaderM r (ReaderT s m) where
  askM = lift askM
  localM f m = ReaderT $ localM f . runReaderT m
  readerM f = lift (readerM f)

instance (MonadReaderM r m) => MonadReaderM r (ExceptT e m) where
  askM = lift askM
  localM = mapExceptT . localM
  readerM = lift . readerM

instance (MonadReaderM r m) => MonadReaderM r (IdentityT m) where
  askM = lift askM
  localM = mapIdentityT . localM
  readerM = lift . readerM

instance (MonadReaderM r m) => MonadReaderM r (MaybeT m) where
  askM = lift askM
  localM = mapMaybeT . localM
  readerM = lift . readerM

instance (MonadReaderM r m) => MonadReaderM r (LoggingT m) where
  askM = lift askM
  localM = mapLoggingT . localM
  readerM = lift . readerM

-- hack to provide a MonadReader instance by a MonadReaderM instance
-- instance {-# OVERLAPS #-} (Monad m, MonadReaderM r m) => MonadReader r m where
--   ask = lift askM
--   local f m = ReaderT $ localM f . runReaderT m
--   reader f = lift (readerM f)

-- data Dict a where
--   Dict :: (a) => Dict a

-- withDict :: (Dict a -> b) -> ((a) => b)
-- withDict f = f Dict

-- readerToM :: forall r m a. ((MonadReader r m) => m a) -> ((MonadReaderM r m) => m a)
-- readerToM m = withDict @(MonadReaderM r m) \dict ->
--   case C.unsafeCoerce @_ @(Dict (MonadReader r m)) dict of
--     Dict -> m

-- -- case C.unsafeCoerce @(Dict (MonadReaderM Int (ReaderTT Int IO)))
-- --   @(Dict (MonadReader Int (ReaderTT Int IO)))
-- --   Dict of
-- --   Dict -> flip runReaderTT (1 :: Int) do
-- --     x <- ask
-- --     pure x

-- newtype ReaderTT r m a = ReaderTT {runReaderTT :: r -> m a}

-- mapReaderTT :: (m a -> n b) -> ReaderTT r m a -> ReaderTT r n b
-- mapReaderTT f m = ReaderTT $ f . runReaderTT m

-- liftReaderTT :: m a -> ReaderTT r m a
-- liftReaderTT m = ReaderTT (const m)

-- instance (Functor m) => Functor (ReaderTT r m) where
--   fmap f = mapReaderTT (fmap f)

-- instance (Applicative m) => Applicative (ReaderTT r m) where
--   pure = liftReaderTT . pure
--   f <*> v = ReaderTT $ \r -> runReaderTT f r <*> runReaderTT v r

-- instance (Monad m) => Monad (ReaderTT r m) where
--   m >>= k = ReaderTT $ \r -> do
--     a <- runReaderTT m r
--     runReaderTT (k a) r

-- instance (Monad m) => MonadReaderM r (ReaderTT r m) where
--   askM = ReaderTT pure
--   localM f m = ReaderTT $ runReaderTT m . f
--   readerM f = ReaderTT (pure . f)

-- test :: IO Int
-- test = flip runReaderTT (1 :: Int) $ readerToM do
--   x <- ask
--   pure x

-- test2 :: (MonadReaderM Int m, MonadReaderM Double m, MonadError Text m) => m Double
-- test2 = do
--   x :: Int <- askM
--   y :: Double <- askM
--   when (x == 0) do
--     throwError "Division by 0"
--   pure (1 / fromIntegral x + y)

-- test3 :: (MonadReader Int m, MonadReader Double m, MonadError Text m) => m Double
-- test3 = do
--   x :: Int <- ask
--   y :: Double <- ask
--   when (x == 0) do
--     throwError "Division by 0"
--   pure (1 / fromIntegral x + y)

-- -- >>> runIdentity (runExceptT (runReaderT (runReaderT test2 (2 :: Int)) (3 :: Double)))
-- -- Right 3.5

-- -- >>> test
-- -- 1
