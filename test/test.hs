{-# LANGUAGE
  DataKinds,
  DeriveGeneric,
  StandaloneDeriving,
  TemplateHaskell,
  TypeApplications,
  TypeOperators
  #-}

import Data.Typeable
import GHC.Generics (Rep)
import Generic.Data.TH
import Generic.Data
import Generic.Data.Microsurgery
import Test.Tasty
import Test.Tasty.HUnit

infixr 1 %%
type (%%) s a = GSurgery s a

data D = D1 Int | D2 Char
  deriving Generic

data R = R
  { f1 :: Int
  , f2 :: Char }
  deriving Generic

type Append2 = SAppend "2"
type Append2Ty r = RenameType Append2 %% RenameConstrs Append2 %% RenameFields Append2 %% r

return []

data_ @(Append2Ty (Rep D))
data_ @(Append2Ty (Rep R))

deriving instance Generic D2
deriving instance Generic R2

main :: IO ()
main = defaultMain $ testGroup "test"
  [ testCase "D2" $ do
      typeRep @_ @(Rep D2) undefined @?= typeRep @_ @(Append2Ty (Rep D)) undefined
  , testCase "R2" $ do
      typeRep @_ @(Rep R2) undefined @?= typeRep @_ @(Append2Ty (Rep R)) undefined
  ]
