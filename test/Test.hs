{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Char (isDigit)
import Language.Haskell.TH
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit

import Database.Beam
import Database.Beam.TH

import TestData

main :: IO ()
main = defaultMainWithIngredients [ consoleTestReporter ] tests

-- To check for equality, remove the uniqueness suffixes (x_12345678) added by the renamer
pruneUniqueVars :: String -> String
pruneUniqueVars = go
  where
    go [] = []
    go (x:xs) = case x of
      '_' -> go . dropWhile isDigit $ xs
      _ -> x : go xs

expectUser, expectOrder :: String
expectUser = "[TySynD User [] (AppT (ConT UserT) (ConT Data.Functor.Identity.Identity)),StandaloneDerivD [] (AppT (ConT GHC.Show.Show) (ConT User)),InstanceD Nothing [] (AppT (ConT Database.Beam.Schema.Tables.Table) (ConT UserT)) [DataInstD [] Database.Beam.Schema.Tables.PrimaryKey [ConT UserT,VarT f] Nothing [NormalC UserId [(Bang NoSourceUnpackedness NoSourceStrictness,AppT (AppT (ConT Database.Beam.Schema.Tables.Columnar) (VarT f)) (ConT Data.Text.Internal.Text))]] [ConT GHC.Generics.Generic],FunD Database.Beam.Schema.Tables.primaryKey [Clause [VarP x] (NormalB (AppE (ConE UserId) (AppE (VarE TestData.userName) (VarE x)))) []]],TySynD UserId [KindedTV f (AppT (AppT ArrowT StarT) StarT)] (AppT (AppT (ConT Database.Beam.Schema.Tables.PrimaryKey) (ConT UserT)) (VarT f)),TySynD UserId' [] (AppT (ConT UserId) (ConT Data.Functor.Identity.Identity)),StandaloneDerivD [] (AppT (ConT GHC.Show.Show) (ConT UserId')),SigD userNameC (AppT (AppT (ConT Lens.Micro.Type.Lens') (AppT (ConT UserT) (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT UserT)))) (AppT (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT UserT)) WildCardT)),ValD (ConP User [ConP Database.Beam.Schema.Tables.LensFor [VarP userNameC]]) (NormalB (VarE Database.Beam.Schema.Lenses.tableConfigLenses)) []]"
expectOrder = "[TySynD Order [] (AppT (ConT OrderT) (ConT Data.Functor.Identity.Identity)),StandaloneDerivD [] (AppT (ConT GHC.Show.Show) (ConT Order)),InstanceD Nothing [] (AppT (ConT Database.Beam.Schema.Tables.Table) (ConT OrderT)) [DataInstD [] Database.Beam.Schema.Tables.PrimaryKey [ConT OrderT,VarT f] Nothing [NormalC OrderId [(Bang NoSourceUnpackedness NoSourceStrictness,AppT (AppT (ConT Database.Beam.Schema.Tables.Columnar) (VarT f)) (ConT Data.Text.Internal.Text))]] [ConT GHC.Generics.Generic],FunD Database.Beam.Schema.Tables.primaryKey [Clause [VarP x] (NormalB (AppE (ConE OrderId) (AppE (VarE TestData.orderItem) (VarE x)))) []]],TySynD OrderId [KindedTV f (AppT (AppT ArrowT StarT) StarT)] (AppT (AppT (ConT Database.Beam.Schema.Tables.PrimaryKey) (ConT OrderT)) (VarT f)),TySynD OrderId' [] (AppT (ConT OrderId) (ConT Data.Functor.Identity.Identity)),StandaloneDerivD [] (AppT (ConT GHC.Show.Show) (ConT OrderId')),SigD orderItemC (AppT (AppT (ConT Lens.Micro.Type.Lens') (AppT (ConT OrderT) (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT OrderT)))) (AppT (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT OrderT)) WildCardT)),SigD orderIssuerC (AppT (AppT (ConT Lens.Micro.Type.Lens') (AppT (ConT OrderT) (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT OrderT)))) (AppT (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT OrderT)) WildCardT)),SigD orderAmountC (AppT (AppT (ConT Lens.Micro.Type.Lens') (AppT (ConT OrderT) (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT OrderT)))) (AppT (AppT (ConT Database.Beam.Schema.Tables.TableField) (ConT OrderT)) WildCardT)),ValD (ConP Order [ConP Database.Beam.Schema.Tables.LensFor [VarP orderItemC],ConP TestData.UserId [ConP Database.Beam.Schema.Tables.LensFor [VarP orderIssuerC]],ConP Database.Beam.Schema.Tables.LensFor [VarP orderAmountC]]) (NormalB (VarE Database.Beam.Schema.Lenses.tableConfigLenses)) []]"

tests :: TestTree
tests = testGroup "Template Haskell boilerpate generation" [ testGroup "Compare Output against Golden Value" [
  testCase "makeTable  ''UserT  'userName" $ do
      let decs = $(stringE . show =<< makeTable ''UserT 'userName)
      assertEqual "" expectUser (pruneUniqueVars decs),
  testCase "makeTable' ''UserT" $ do
      let decs = $(stringE . show =<< makeTable' ''UserT)
      assertEqual "" expectUser (pruneUniqueVars decs),
  testCase "makeTable  ''OrderT 'orderItem" $ do
      let decs = $(stringE . show =<< makeTable ''OrderT 'orderItem)
      assertEqual "" expectOrder (pruneUniqueVars decs),
  testCase "makeTable' ''OrderT" $ do
      let decs = $(stringE . show =<< makeTable' ''OrderT)
      assertEqual "" expectOrder (pruneUniqueVars decs)
           ]]
