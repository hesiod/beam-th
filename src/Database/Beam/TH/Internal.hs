{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TupleSections #-}
module Database.Beam.TH.Internal (
  MakeTableT(..), MakeTable, MakeTableT', MakeTableT'',
  runTableT, tellD,
  rename,
  vst, name, nameId, nameId', nameT,
  (<~>), (<+>), (~>),
  assert, assertMany, invalidConstructor
  ) where

import Control.Monad (unless)
import qualified Control.Monad.Fail as Fail (MonadFail(..))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT(..), MonadReader, asks)
import Control.Monad.Writer (WriterT(..), MonadWriter, execWriterT, tell)
import Control.Monad.Trans (MonadTrans(..))
import Data.Foldable (traverse_)
import Language.Haskell.TH (Name, mkName, nameBase, reportError, Q, DecsQ, Dec, Type(..), Exp(..))
import Language.Haskell.TH.Syntax (VarBangType)
import Data.Typeable (Typeable)

{-# INLINE rename #-}
rename :: (String -> String) -> Name -> Name
rename f = mkName . f . nameBase

{-# INLINE mkNameSelector #-}
mkNameSelector :: MonadReader (Name, VarBangType) m => String -> m Name
mkNameSelector suffix = asks (rename (++ suffix) . fst)
{-# INLINE vst #-}
vst :: MonadReader (Name, VarBangType) m => m VarBangType
vst = asks snd
{-# INLINE name #-}
{-# INLINE nameId #-}
{-# INLINE nameId' #-}
{-# INLINE nameT #-}
name, nameId, nameId', nameT :: MonadReader (Name, VarBangType) m => m Name
name = asks fst
nameId = mkNameSelector "Id"
nameId' = mkNameSelector "Id'"
nameT = mkNameSelector "T"


newtype MakeTableT m a = MakeTableT { runTable :: WriterT [Dec] (ReaderT (Name, VarBangType) m) a }
                     deriving (Typeable, Functor, Applicative, Monad, MonadReader (Name, VarBangType), MonadWriter [Dec], Fail.MonadFail)
instance MonadTrans MakeTableT where
  lift = MakeTableT . lift . lift
type MakeTable a = MakeTableT Identity a
type MakeTableT' a = MakeTableT Q a
type MakeTableT'' = MakeTableT' ()
{-
instance Monoid MakeTableT'' where
  mempty = MakeTableT . WriterT . lift . pure $ ((), [])
  mappend = curry (mkt <=< (uncurry ((<*>) . (mappend <$>)) . (f *** f)))
    where
      f = fmap snd . MakeTableT . lift . runWriterT . runTable
      mkt = MakeTableT . WriterT . ReaderT . const . pure . ((),)
-}
{-# INLINE runTableT #-}
runTableT :: Name -> VarBangType -> MakeTableT' a -> DecsQ
runTableT n v = flip runReaderT (n, v) . execWriterT . runTable

{-# INLINE tellD #-}
tellD :: MonadWriter [Dec] m => Dec -> m ()
tellD = tell . pure

{-# INLINE (<~>) #-}
(<~>) :: Type -> Type -> Type
a <~> b = AppT a b

{-# INLINE (<+>) #-}
(<+>) :: Exp -> Exp -> Exp
a <+> b = AppE a b

{-# INLINE (~>) #-}
(~>) :: Type -> Type -> Type
a ~> b = ArrowT <~> a <~> b

infixl 4 <~>, <+>, ~>

{-# INLINE assert #-}
assert :: Bool -> String -> Q ()
assert cond msg = unless cond (reportError $ "Table name does not follow convention: " ++ msg ++ "; use 'MyTableNameT' or so")
{-# INLINE assertMany #-}
assertMany :: [(Bool, String)] -> Q ()
assertMany = traverse_ (uncurry assert)

{-# INLINE invalidConstructor #-}
invalidConstructor :: Fail.MonadFail m => m a
invalidConstructor = Fail.fail "Invalid constructor field; the primary key must be of the form 'Columnar f SomeType'"
