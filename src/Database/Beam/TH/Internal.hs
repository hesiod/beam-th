{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- | Various internal utilities for beam-th. The usual caveats apply.
module Database.Beam.TH.Internal (
  -- * The MakeTableT monad transformer
  -- ** Definition
  MakeTableT(..),
  -- ** Derived type synonyms
  MakeTable, MakeTableT', MakeTableT'',
  -- ** Helper functions
  runTableT, tellD,
  -- ** Extracting values from a MakeTableT
  vst,
  -- *** Simple and composite names
  name, nameId, nameId', nameT,
  -- * Name utilities
  rename,
  -- * Type and Expression Application Sugar
  (<~>), (<+>), (~>),
  -- * Error handling
  -- ** Constructor types
  invalidConstructor,
  -- ** Table names
  assert, assertMany
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

-- | Rename a 'Name' using a function on 'String's
rename :: (String -> String) -> Name -> Name
rename f = mkName . f . nameBase
{-# INLINE rename #-}

mkNameSelector :: MonadReader (Name, VarBangType) m => String -> m Name
mkNameSelector suffix = asks (rename (++ suffix) . fst)
{-# INLINE mkNameSelector #-}

-- | Extract the 'PrimaryKey' 'VarBangType'
vst :: MonadReader (Name, VarBangType) m => m VarBangType
vst = asks snd
{-# INLINE vst #-}

name, nameId, nameId', nameT :: MonadReader (Name, VarBangType) m => m Name
-- | Get the base name
name = asks fst
-- | Get the name with an \"Id\" suffix
nameId = mkNameSelector "Id"
-- | Get the name with an \"Id'\" suffix
nameId' = mkNameSelector "Id'"
-- | Get the name with a \"T\" suffix
nameT = mkNameSelector "T"
{-# INLINE name #-}
{-# INLINE nameId #-}
{-# INLINE nameId' #-}
{-# INLINE nameT #-}


-- | A monad transformer for writing Template Haskell declarations.
--
-- The Reader contains both the base name of the table and the 'VarBangType'
-- of the primary key field.
--
-- If you can come up with a better name, drop me a line.
newtype MakeTableT m a = MakeTableT { runTable :: WriterT [Dec] (ReaderT (Name, VarBangType) m) a }
                     deriving (Typeable, Functor, Applicative, Monad, MonadReader (Name, VarBangType), MonadWriter [Dec], Fail.MonadFail)
instance MonadTrans MakeTableT where
  lift = MakeTableT . lift . lift
-- | Type synonym for 'MakeTableT' in the 'Identity' monad.
--
-- Only defined for complying with the monad transformer conventions
-- and not actually used.
type MakeTable a = MakeTableT Identity a
-- | Type synonym for 'MakeTableT' in the 'Q' monad.
type MakeTableT' a = MakeTableT Q a
-- | Type synonym for 'MakeTableT' in the 'Q' monad with the empty tuple as the inner type.
-- This is the most common use case.
type MakeTableT'' = MakeTableT' ()

{-
instance Monoid MakeTableT'' where
  mempty = MakeTableT . WriterT . lift . pure $ ((), [])
  mappend = curry (mkt <=< (uncurry ((<*>) . (mappend <$>)) . (f *** f)))
    where
      f = fmap snd . MakeTableT . lift . runWriterT . runTable
      mkt = MakeTableT . WriterT . ReaderT . const . pure . ((),)
-}

-- | Run the table writing sequence (or, the 'MakeTableT' if you prefer).
runTableT ::
  Name             -- ^ The base name of the table, without the trailing \"T\".
  -> VarBangType   -- ^ The primary key field.
  -> MakeTableT' a -- ^ The table writing sequence to be executed. The inner type is ignored.
  -> DecsQ
runTableT n v = flip runReaderT (n, v) . execWriterT . runTable
{-# INLINE runTableT #-}

-- | Write a single 'Dec'
tellD :: MonadWriter [Dec] m => Dec -> m ()
tellD = tell . pure
{-# INLINE tellD #-}

-- $setup
-- >>> let nm = mkName "nm"

-- | Convenient syntactic sugar for application of types.
--
-- >>> ConT nm <~> ConT nm <~> ConT nm
-- AppT (AppT (ConT nm) (ConT nm)) (ConT nm)
(<~>) :: Type -> Type -> Type
a <~> b = AppT a b
{-# INLINE (<~>) #-}

-- | Convenient syntactic sugar for application of expressions.
--
-- >>> ConE nm <+> ConE nm <+> ConE nm
-- AppE (AppE (ConE nm) (ConE nm)) (ConE nm)
(<+>) :: Exp -> Exp -> Exp
a <+> b = AppE a b
{-# INLINE (<+>) #-}

-- | Convenient syntactic sugar for arrows in types.
--
-- >>> StarT ~> StarT
-- AppT (AppT ArrowT StarT) StarT
(~>) :: Type -> Type -> Type
a ~> b = ArrowT <~> a <~> b
{-# INLINE (~>) #-}

infixl 6 <~>, <+>, ~>

-- | Assert a condition related to the table base name and suggest following the naming convention.
assert :: Bool -> String -> Q ()
assert cond msg = unless cond (reportError $ "Table name does not follow convention: " ++ msg ++ "; use 'MyTableNameT' or so")
-- | Assert a list of conditions and associated error messages.
assertMany :: [(Bool, String)] -> Q ()
assertMany = traverse_ (uncurry assert)

-- | Complain about an unknown field in the table.
invalidConstructor :: Fail.MonadFail m => m a
invalidConstructor = Fail.fail "Invalid constructor field; the primary key must be of the form 'Columnar f SomeType'"
