{-# LANGUAGE
  AllowAmbiguousTypes,
  DuplicateRecordFields,
  FlexibleInstances,
  NoFieldSelectors,
  OverloadedRecordDot,
  ScopedTypeVariables,
  TemplateHaskell,
  TypeApplications,
  TypeFamilies,
  TypeOperators
  #-}

-- | This is an internal module. The public API is 'Generic.Data.TH'.
module Generic.Data.Internal.TH where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import LiftType (liftType)
import Language.Haskell.TH (Q, Bang(..), Dec(DataD, NewtypeD), Con(..), mkName)
import qualified Language.Haskell.TH as TH
import GHC.Generics (M1, D, C, S, U1, V1, (:*:), (:+:), K1)
import qualified GHC.Generics as GHC

-- |
-- @
-- data_ \@()
-- @
data_ :: forall r. ToDataDecl r => Q [Dec]
data_ = pure (dataDecl (toDataDecl @r))

dataDecl :: DataDecl -> [Dec]
dataDecl d | d.isNewtype =
  case d.constructors of
    [c] -> [NewtypeD [] (mkName d.name) [] Nothing (conDecl c) []]
    _ -> error "newtype must have only one constructor"
dataDecl d =
  [DataD [] (mkName d.name) [] Nothing (fmap conDecl d.constructors) []]

conDecl :: ConDecl -> Con
-- TODO: infix, records
conDecl c | f : _ <- c.fields, Just _ <- f.name =
  RecC (mkName c.name) (fmap recFieldDecls c.fields)
conDecl c = NormalC (mkName c.name) (fmap fieldDecls c.fields)

fieldDecls :: FieldDecl -> (Bang, TH.Type)
fieldDecls f = (Bang f.unpack f.strict, f.ty)

recFieldDecls :: FieldDecl -> (TH.Name, TH.Bang, TH.Type)
recFieldDecls f = case f.name of
  Nothing -> error "record field without a name"
  Just n -> (mkName n, Bang f.unpack f.strict, f.ty)

class ToDataDecl (r :: Type -> Type) where
  toDataDecl :: DataDecl

instance (GHC.Datatype d, ToConstructors f) => ToDataDecl (M1 D d f) where
  toDataDecl = DataDecl
    { name = GHC.datatypeName (undefined :: M1 D d f x)
    , isNewtype = GHC.isNewtype (undefined :: M1 D d f x)
    , constructors = toConstructors @f []
    }

class ToConstructors (r :: Type -> Type) where
  -- Difference list
  toConstructors :: [ConDecl] -> [ConDecl]

instance (ToConstructors f, ToConstructors g) => ToConstructors (f :+: g) where
  toConstructors = toConstructors @f . toConstructors @g

instance ToConstructors V1 where
  toConstructors = id

instance (GHC.Constructor c, ToFields f) => ToConstructors (M1 C c f) where
  toConstructors = (:) ConDecl
    { name = GHC.conName (undefined :: M1 C c f x)
    , fixity = toTHFixity (GHC.conFixity (undefined :: M1 C c f x))
    , fields = toFields @f []
    }

toTHFixity :: GHC.Fixity -> Maybe TH.Fixity
toTHFixity GHC.Prefix = Nothing
toTHFixity (GHC.Infix assoc lvl) = Just (TH.Fixity lvl (toTHFixityDirection assoc))

toTHFixityDirection :: GHC.Associativity -> TH.FixityDirection
toTHFixityDirection GHC.NotAssociative = TH.InfixN
toTHFixityDirection GHC.LeftAssociative = TH.InfixL
toTHFixityDirection GHC.RightAssociative = TH.InfixR

class ToFields (r :: Type -> Type) where
  toFields :: [FieldDecl] -> [FieldDecl]

instance (ToFields f, ToFields g) => ToFields (f :*: g) where
  toFields = toFields @f . toFields @g

instance ToFields U1 where
  toFields = id

instance (GHC.Selector s, Typeable a) => ToFields (M1 S s (K1 i a)) where
  toFields = (:) FieldDecl
    { name = case GHC.selName (undefined :: M1 S s (K1 i a) x) of "" -> Nothing ; n -> Just n
    , unpack = toTHUnpackedness (GHC.selSourceUnpackedness (undefined :: M1 S s (K1 i a) x))
    , strict = toTHStrictness (GHC.selSourceStrictness (undefined :: M1 S s (K1 i a) x))
    , ty = liftType @a
    }

toTHUnpackedness :: GHC.SourceUnpackedness -> TH.SourceUnpackedness
toTHUnpackedness GHC.NoSourceUnpackedness = TH.NoSourceUnpackedness
toTHUnpackedness GHC.SourceNoUnpack = TH.SourceNoUnpack
toTHUnpackedness GHC.SourceUnpack = TH.SourceUnpack

toTHStrictness :: GHC.SourceStrictness -> TH.SourceStrictness
toTHStrictness GHC.NoSourceStrictness = TH.NoSourceStrictness
toTHStrictness GHC.SourceLazy = TH.SourceLazy
toTHStrictness GHC.SourceStrict = TH.SourceStrict

data DataDecl = DataDecl
  { name :: String
  , isNewtype :: Bool
  , constructors :: [ConDecl]
  }

data ConDecl = ConDecl
  { name :: String
  , fixity :: Maybe TH.Fixity
  , fields :: [FieldDecl]
  }

data FieldDecl = FieldDecl
  { name :: Maybe String
  , unpack :: TH.SourceUnpackedness
  , strict :: TH.SourceStrictness
  , ty :: TH.Type
  }
