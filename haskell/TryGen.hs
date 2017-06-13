{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

module TryGen ( explode, consolidate, Bar(..)) where

import GHC.Generics
import Data.Typeable
import Data.Text ( pack, unpack, Text)


-- | This the the function that converts things to kv pairs. Ignore
-- the second component of the constraint.
explode :: (Generic a, Record (Rep a (Proxy a))) =>  a -> KVPairs
explode =  toKVs . getMeta
  where getMeta :: Generic a => a -> Rep a (Proxy a)
        getMeta = from

-- | This functon consolidates kvPairs to a record.
consolidate :: (Generic a, Record (Rep a (Proxy a)))
            => KVPairs
            -> Maybe a
consolidate kvs =  do (v, rest) <- runRecParser (thisParser recParser) kvs
                      if null rest then return v else Nothing
  where thisParser :: Generic a => RecParser (Rep a (Proxy a)) -> RecParser a
        thisParser = fmap to


---------------------- These type classes are the important stuff ----------------

-- | Things that are values.
class Value a where
  parse     :: Text -> Maybe a
  serialise :: a    -> Text

  -- These default instances makes it easier for defining standard
  -- value types. (See that at the end of the file).
  default parse :: Read a  => Text -> Maybe a
  parse txt | null rs   = Nothing
            | otherwise = Just $ fst $ head rs
    where rs = readsPrec 1 $ unpack txt

  default serialise :: Show a  => a -> Text
  serialise = pack . show


-- | Things that have a key.
class Key a where
  -- | Recover the key. We give a proxy because, this function should
  -- not look at the value.
  key   :: Proxy a -> Text


-- | Make the kv pair for a type having a key and a value.
pair :: (Value a, Key a) => a -> (Text, Text)
pair a = (key $ proxy a, serialise a)


class Record a where
  -- | Make an element out of the key value pairs. The RecParser type
  -- is defined at the end.
  recParser :: RecParser a

  -- Convert the value into KV pairs.
  toKVs    :: a -> KVPairs

  default recParser :: (Key a, Value a) => RecParser a
  recParser = thisParser
    where fname      = key $ recParserProxy thisParser
          thisParser = joinMaybe $ parse <$> field fname


  default toKVs :: (Key a, Value a) => a -> KVPairs
  toKVs a = [pair a]

-- | Product of records are records. 
instance (Record (f p), Record (g p) ) => Record ((f :*: g) p) where
  toKVs (x :*: y)   = toKVs x ++ toKVs y
  recParser = (:*:) <$> recParser <*> recParser


-------------  Key, Value, Record instances for Rep types ----------------------------

-- | Lifting value instance to the record level.
instance Value a => Value (K1 R a p) where
  parse     = fmap K1   . parse
  serialise = serialise . unK1

---------------------------------- Selectors ------------------------------------------

-- | Lift value to Record Selector level.
instance Value (f p) => Value (M1 S c f p) where
  parse     = fmap M1   . parse
  serialise = serialise . unM1

-- | In the record level we use a selector type to capture the record
-- level selector.
instance Selector c => Key (M1 S c f p) where
  key     = pack      . selName . metaProxy

-- | Make the selector type a record. The default instance makes this pretty convenient.
instance (Value (f p), Selector c) => Record (M1 S c f p)

------------------------------ Constructors ------------------------------------------

instance (Record (f p)) => Record (M1 C c f p) where
  toKVs     = toKVs . unM1
  recParser = M1 <$> recParser

----------------------------- Datatypes ----------------------------------------------

instance (Record (f p)) => Record (M1 D c f p) where
  toKVs     = toKVs . unM1
  recParser = M1 <$> recParser

------------------------ Record Parsering      ---------------------------------------

-- | A records representation is as key value pairs.
type KVPairs = [ (Text, Text) ]


-- | Not necessarily required but to clean things up.
newtype RecParser a = RecParser { runRecParser :: KVPairs -> Maybe (a, KVPairs) }


-- | This is like the join for the monad but lifts a Maybe.
joinMaybe :: RecParser (Maybe a) -> RecParser a
joinMaybe rp = RecParser $ \ kvs -> do (maybeV,rest) <- runRecParser rp kvs
                                       x <- maybeV
                                       return (x, rest)
                                       
instance Functor RecParser  where
  fmap f recP = RecParser $ \ kvs -> do (v, rest) <- runRecParser recP kvs
                                        return (f v, rest)

instance Applicative RecParser where
  pure a    = RecParser $ \ kvs  -> Just (a, kvs)
  p1 <*> p2 = RecParser $ \ inp1 -> do (f, inp2) <- runRecParser p1 inp1
                                       (a, rest) <- runRecParser p2 inp2
                                       return (f a , rest)

-- | Exercise can you define Monad instance.

-- | Get the value associate with a key and the rest of the records.
field  :: Text -> RecParser Text
field k = RecParser action
  where matcher   = (==k) . fst
        action kvs = case suf of
          []           -> Nothing
          ((_,v):rest) -> Just (v, pre ++ rest)
          where (pre,suf) = break matcher kvs 



---------------------- Value instance for some basic types -------------------------

instance Value String where
  parse     = Just . unpack
  serialise = pack

instance Value Text where
  parse     = Just
  serialise = id

instance Value Int

instance Value Double


------------ Some helper proxies. ----------------------------

recParserProxy :: RecParser a -> Proxy a
recParserProxy _ = Proxy
-- | Useful to get only the meta information.
metaProxy :: Proxy (M1 i c f p) -> M1 i c Proxy p
metaProxy _ = M1 Proxy

-- | Recover the proxy from an element.
proxy :: a -> Proxy a
proxy _ = Proxy


--------------------- A Type to test things --------------------------

data Bar = Bar { barI :: Int , barS :: String, barD :: Double} deriving (Show, Generic)
