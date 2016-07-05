{-# LANGUAGE TemplateHaskell, KindSignatures, StandaloneDeriving, TypeFamilies, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
module TestData where

import Data.Text (Text)
import Database.Beam
import Database.Beam.TH

data UserT f = User {
      userName       :: Columnar f Text
    } deriving (Generic)
makeTable' ''UserT

data OrderT f = Order {
      orderItem      :: Columnar f Text,
      orderIssuer    :: UserId f,
      orderRecipient :: PrimaryKey UserT f,
      orderAmount    :: Columnar f Int
    } deriving (Generic)
makeTable' ''OrderT
