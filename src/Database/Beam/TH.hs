{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase #-}
-- | Functions to derive common boilerplate code when writing table types for the <https://hackage.haskell.org/package/beam beam>
-- library. Only use them if you what you're doing.
--
-- The following GHC extensions have to be enabled in order to make the generated code typecheck:
--
-- @
-- {-\# LANGUAGE TemplateHaskell, KindSignatures, StandaloneDeriving, TypeFamilies, TypeSynonymInstances, FlexibleInstances, DeriveGeneric \#-}
-- @
module Database.Beam.TH (makeTable, makeTable') where

import Data.Maybe (fromJust)
import Control.Monad ((>=>), forM)
import Control.Monad.Identity (Identity)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Writer (WriterT, execWriterT)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.ExpandSyns (expandSyns)

import Database.Beam (Table, TableField, Columnar, PrimaryKey, primaryKey, tableConfigLenses, LensFor(..))
import Database.Beam.TH.Internal
import Lens.Micro (Lens')

{-# INLINE getColTy #-}
getColTy :: Type -> Q Name
getColTy (AppT (AppT (ConT _) (VarT _)) (ConT tyInner)) = pure tyInner
getColTy _ = invalidConstructor

primaryKeyTy, primaryKeyFun :: WriterT [Dec] (MakeTableT Q) ()
primaryKeyTy = do
  (_, str, ty) <- vst
  nmT <- nameT
  nmI <- nameId
  f <- lift . lift $ newName "f"
  cty <- lift . lift $ getColTy ty
  let primaryKeyTyRHS = pure (str, ConT ''Columnar <~> VarT f <~> ConT cty)
  tellD $ DataInstD [] ''PrimaryKey [ConT nmT, VarT f] Nothing [NormalC nmI primaryKeyTyRHS] [ConT ''Generic]
primaryKeyFun = do
  (pk, _, _) <- vst
  nmI <- nameId
  x <- lift . lift $ newName "x"
  tellD . FunD 'primaryKey . pure $ Clause [VarP x] (NormalB (ConE nmI <+> (VarE pk <+> VarE x))) []
nameInst, nameTySyn, nameIdTySyn, nameLens :: MakeTableT''
nameInst = do
  nmT <- nameT
  decs <- execWriterT $ primaryKeyTy >> primaryKeyFun
  tellD $ InstanceD Nothing [] (ConT ''Table <~> ConT nmT) decs
nameTySyn = do
  nm <- name
  nmT <- nameT
  tellD . TySynD nm []        $ ConT nmT <~> ConT ''Identity
  tellD . StandaloneDerivD [] $ ConT ''Show <~> ConT nm
nameIdTySyn = do
  nmT <- nameT
  nmId <- nameId
  nmId' <- nameId'
  f <- lift $ newName "f"
  tellD . TySynD nmId  [KindedTV f (StarT ~> StarT)] $ ConT ''PrimaryKey <~> ConT nmT <~> VarT f
  tellD . TySynD nmId' []                            $ ConT nmId <~> ConT ''Identity
  tellD . StandaloneDerivD []                        $ ConT ''Show <~> ConT nmId'
nameLens = do
  nm <- name
  nmT <- nameT
  (Just lf) <- lift . lookupValueName $ "LensFor"
  (TyConI (DataD _ _ _ _ (RecC _ vsts:_) _)) <- lift $ reify nmT
  let fields = fmap renameFields vsts
      signature x = tellD . SigD x $ ConT lens' <~> (ConT nmT <~> (ConT ''TableField <~> ConT nmT))
                                                <~> (ConT ''TableField <~> ConT nmT <~> WildCardT)
  fields' <- forM fields (\(x, t) -> lift (opportunisticExpand t) >>= \case
                             AppT (AppT (ConT test) (ConT _)) _ | test == ''PrimaryKey -> do
                                                                    signature x
                                                                    c <- lift . extractCon $ t
                                                                    pure . ConP c . pure . ConP lf . pure . VarP $ x
                             _ -> do
                               signature x
                               pure . ConP lf . pure . VarP $ x)
  tellD $ ValD (ConP nm fields') (NormalB (VarE 'tableConfigLenses)) []
    where
      renameFields (cname, _, t) = (rename (++ "C") cname, t)
      lens' = ''Lens'
      extractCon (AppT (ConT c) _) = fmap fromJust . lookupValueName . nameBase $ c
      extractCon (AppT (AppT _ (ConT c)) _) = fmap fromJust . lookupValueName . (++ "Id") . nameBase =<< baseName c
      extractCon x = error $ "Unknown cross-table reference '" ++ pprint x ++ "'; use PrimaryKey OtherTableT f or the synonymous OtherTableId f"
      -- To circumvent th-expand-syns: WARNING: Type synonym families (and associated type synonyms) are currently not supported (they won't be expanded). Name of unsupported family: Database.Beam.Schema.Tables.Columnar
      opportunisticExpand t@(AppT (AppT (ConT x) _ ) _) | x == ''Columnar = pure t
      opportunisticExpand x = expandSyns x

baseName :: Name -> Q Name
baseName nmT = do
  let nbT = nameBase nmT
      len = length nbT
  assertMany [
    (len >= 3, "Too short"),
    (last nbT == 'T', "Table name does not end with 'T'")
    ]
  pure . mkName . take (pred len) $ nbT

makeTableWithType :: Name -> VarStrictType -> DecsQ
makeTableWithType nmT v = do
  nm <- baseName nmT
  fmap concat . mapM (runTableT nm v) $ [nameTySyn, nameInst, nameIdTySyn, nameLens]

recordFields :: Info -> [VarBangType]
recordFields (TyConI (DataD _ _ _ _ (RecC _ x:_) _)) = x
recordFields _ = invalidConstructor

-- | Derives boilerplate code for beam table types.
--
-- 'makeTable' is equivalent to 'makeTable'' except that 'makeTable' takes a second argument,
-- the name of the primary key, while 'makeTable'' automatically makes the first field of the record
-- the primary key.
--
-- > makeTable ''UserT 'userNumber == makeTable' ''UserT
makeTable ::
  Name      -- ^ The table type name. It should end with \"T\", otherwise the derived names will be bogus.
  -> Name   -- ^ The primary key field name
  -> DecsQ
makeTable nmT nmPk = do
  i <- reify nmT
  makeTableWithType nmT . head . filter (\(nm, _, _) -> nm == nmPk) . recordFields $ i

{-# INLINE firstRecord #-}
firstRecord :: Name -> VarBangTypeQ
firstRecord = reify >=> (pure . head . recordFields)

-- | Derives boilerplate code for beam table types.
--
-- >>> :set -XTemplateHaskell
-- >>> data UserT f = User { userNumber :: Columnar f Int }
-- >>> putStrLn $(stringE . pprint =<< makeTable' ''UserT)
-- type User = UserT Data.Functor.Identity.Identity
-- deriving instance GHC.Show.Show User
-- instance Database.Beam.Schema.Tables.Table UserT
--     where data Database.Beam.Schema.Tables.PrimaryKey UserT f_0
--               = UserId (Database.Beam.Schema.Tables.Columnar f_0 GHC.Types.Int)
--               deriving GHC.Generics.Generic
--           Database.Beam.Schema.Tables.primaryKey x_1 = UserId (Ghci4.userNumber x_1)
-- type UserId (f_2 :: * ->
--                     *) = Database.Beam.Schema.Tables.PrimaryKey UserT f_2
-- type UserId' = UserId Data.Functor.Identity.Identity
-- deriving instance GHC.Show.Show UserId'
-- userNumberC :: Lens.Micro.Type.Lens' (UserT (Database.Beam.Schema.Tables.TableField UserT))
--                                      (Database.Beam.Schema.Tables.TableField UserT _)
-- User (Database.Beam.Schema.Tables.LensFor userNumberC) = Database.Beam.Schema.Lenses.tableConfigLenses
--
-- Note: While the above example actually is a valid doctest, due to variable renaming and the pretty printer
-- having a line break deficit it looks rather confusing. Therefore, consider the following reformatted but
-- otherwise equivalent example:
--
-- @
-- type User = UserT Identity
-- deriving instance Show User
-- instance Table UserT where
--     data PrimaryKey UserT f = UserId (Columnar f Int) deriving Generic
--     primaryKey = UserId . userNumber
-- type UserId f = PrimaryKey UserT f
-- type UserId' = UserId Identity
-- deriving instance Show UserId'
-- userNumberC :: Lens' (UserT (TableField UserT)) (TableField UserT Int)
-- User (LensFor userNumberC) = tableConfigLenses
-- @
--
-- Note that the @UserId@ type synonym is very useful when refering to other tables in fields. Consider this:
-- @
-- data BlogPostT f = BlogPost { blogPostId :: Columnar f Int, blogPostAuthor :: UserId f }
-- @
--
-- 'makeTable' is equivalent to 'makeTable'' except that 'makeTable' takes a second argument,
-- the name of the primary key, while 'makeTable'' automatically makes the first field of the record
-- the primary key.
--
-- > makeTable ''UserT 'userNumber == makeTable' ''UserT
makeTable' ::
  Name     -- ^ The table type name. It should end with \"T\", otherwise the derived names will be bogus.
  -> DecsQ
makeTable' nm = do
  fr <- firstRecord nm
  makeTableWithType nm fr
