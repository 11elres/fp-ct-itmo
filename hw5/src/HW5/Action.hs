module HW5.Action
  ( HIO(..)
  , HiPermission(..)
  , PermissionException(..)
  ) where

import           Control.Applicative       ((<|>))
import           Control.Exception         (Exception)
import           Control.Exception.Base    (throwIO)
import           Control.Monad             (ap)
import           Control.Monad.Combinators (optional)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as ByteString
import qualified Data.Sequence             as Seq
import           Data.Set                  (Set)
import qualified Data.Text                 as Text
import           Data.Text.Encoding        (decodeUtf8')
import           Data.Time.Clock           (getCurrentTime)
import           System.Directory          (createDirectory,
                                            getCurrentDirectory, listDirectory,
                                            setCurrentDirectory)
import           System.Random             (randomRIO)

import           HW5.Base                  (HiAction (..), HiMonad (..),
                                            HiValue (..))

-- | Permission that can be required when running 'HiAction'.
data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Enum, Ord, Eq)

-- | 'PermissionException' contains information about problems with permissions,
-- that occur when running 'HIAction'.
data PermissionException =
  PermissionRequired HiPermission -- ^ Specified 'HiPermission' is not allowed
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f (HIO runA) = HIO $ fmap f . runA

instance Applicative HIO where
  pure a = HIO $ const $ pure a
  (<*>) = Control.Monad.ap

instance Monad HIO where
  (HIO runA) >>= f = HIO $ \perms -> do
    a <- runA perms
    runHIO (f a) perms

instance HiMonad HIO where
  runAction (HiActionRead fp)      = HIO $ requireAndAction AllowRead $ fromFile <$> ByteString.readFile fp
                                                                    <|> fromDir  <$> listDirectory fp
    where
      fromFile :: ByteString -> HiValue
      fromFile bytes = either (const $ HiValueBytes bytes) HiValueString (decodeUtf8' bytes)
      fromDir :: [FilePath] -> HiValue
      fromDir = (HiValueList . Seq.fromList) . map (HiValueString . Text.pack)
  runAction (HiActionWrite fp bs)  = HIO $ requireAndAction AllowWrite $ HiValueNull <$ ByteString.writeFile fp bs
  runAction (HiActionMkDir fp)     = HIO $ requireAndAction AllowWrite $ HiValueNull <$ optional (createDirectory fp)
  runAction (HiActionChDir fp)     = HIO $ requireAndAction AllowRead  $ HiValueNull <$ setCurrentDirectory fp
  runAction HiActionCwd            = HIO $ requireAndAction AllowRead  $ HiValueString . Text.pack <$> getCurrentDirectory
  runAction HiActionNow            = HIO $ requireAndAction AllowTime  $ HiValueTime <$> getCurrentTime
  runAction (HiActionRand from to) = HIO $ const $ HiValueNumber . toRational <$> randomRIO (from, to)
  runAction (HiActionEcho s)       = HIO $ requireAndAction AllowWrite $ HiValueNull <$ putStrLn (Text.unpack s)

-- | Runs action if 'HiPermission' is allowed
requireAndAction :: HiPermission -> IO HiValue -> Set HiPermission -> IO HiValue
requireAndAction perm action perms = if perm `elem` perms then action else requirePermission perm

-- | Throws 'PermissionException'
requirePermission :: HiPermission -> IO a
requirePermission = throwIO . PermissionRequired
