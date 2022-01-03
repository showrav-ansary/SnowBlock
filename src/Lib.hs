{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Crypto.Hash.SHA256     as SHA256
import           Data.Aeson             hiding (json)
import           Data.ByteString        (ByteString, append, pack)
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.UTF8   (fromString, toString)
import qualified Data.Word
import           GHC.Generics
import           Unsafe.Coerce          (unsafeCoerce)

data Transaction = Transaction { sender   :: String
                               , receiver     :: String
                               , description :: String
                               } deriving (Generic, Show)

data Block = Block { index    :: Int  -- Index of the block
                   , trnx      :: [Transaction] -- List of transactions in the block
                   , hash     :: String -- Hash of the block
                   , previousHash :: String -- Prev Hash of the block
                   , nonce    :: Maybe Int -- Nonce of the block (proof of work)
                   } deriving (Generic, Show)

instance ToJSON Transaction

instance FromJSON Transaction

instance ToJSON Block

instance FromJSON Block

-- Hash (SHA256) our Block
hashBlock :: Block -> String
hashBlock (Block blockIndex blockTransaction blockHash previousHash _) = toString $ Base16.encode digest
    where blockTransaction = foldr ((++) . show) "" blockTransaction
          context = SHA256.updates SHA256.init $ fmap fromString [blockTransaction, previousHash]
          digest = SHA256.finalize context

-- Adds a transaction to the head of the block
addBlockTransaction :: Block -> Transaction -> Block
addBlockTransaction (Block i ts h p n) t = Block i (ts ++ [t]) h p n

-- Mine the block
mineBlock :: Block -> Int -> Block
mineBlock b@(Block i t _ p _) n = case head pow of
                                    '0' -> Block i t blockHash p (Just n)
                                    _   -> mineBlock b (n + 1)
    where blockHash = hashBlock b
          context = SHA256.updates SHA256.init (fmap fromString [blockHash, show n, p])
          pow = toString . Base16.encode $ SHA256.finalize context -- proof of work


-- Our genesis block
genesisBlock :: Block
genesisBlock = Block blockIndex blockTransaction blockHash previousHash Nothing
    where blockIndex = 0
          blockTransaction = [Transaction "Showrav" "Snow" "What a fluffy ball you are!"]
          blockHash = ""
          previousHash = "000000000000000000000000000000000"