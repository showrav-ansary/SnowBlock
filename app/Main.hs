{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Main where

import qualified Control.Monad.State as S
import           Data.Aeson          hiding (json)
import qualified Data.ByteString
import           Data.IORef
import           Data.Monoid         ((<>))
import           Data.String         (fromString)
import           Data.Text           (Text, pack)
import           GHC.Generics
import           Lib
import           Web.Spock.Config
import           Web.Spock.Core

newtype RunStateT s m = RunStateT { runStateT :: forall a. S.StateT s m a -> m a }

type Blockchain = [Block]


-- Adds a transaction
addTransaction :: (Monad m) => Transaction -> S.StateT Blockchain m Blockchain
addTransaction tx = do
    blockChain <- S.get
    let blockHead = Prelude.head blockChain -- Block head
    let blockTail = Prelude.tail blockChain -- Block tail
    let blockHead' = addBlockTransaction blockHead tx -- Add tx to the head of block
    let blockChain' = ([blockHead'] ++ blockTail) -- New blockchain
    S.put blockChain'
    return [blockHead']


-- Mines the block head
--
mine :: (Monad m) => S.StateT Blockchain m Blockchain
mine = do
    blockChain <- S.get -- Blockchain
    let blockHead = Prelude.head blockChain -- Block head
    let blockTail = Prelude.tail blockChain -- Block tail
    let minedBlock = mineBlock blockHead 0 -- Mined block
    let blockHead' = Block (index minedBlock) [] "" (hash minedBlock) Nothing -- New block head
    let blockChain' = ([blockHead', minedBlock] ++ blockTail)
    S.put blockChain'
    return [minedBlock]


-- Returns current block head
current :: (Monad m) => S.StateT Blockchain m Blockchain
current = do
    blockChain <- S.get
    return $ [Prelude.head blockChain]


-- Gets current blockhash
getBlockhash :: (Monad m) => String -> S.StateT Blockchain m Blockchain
getBlockhash h = do
    blockChain <- S.get
    return $ filter (\b -> (hash b) == h) blockChain


-- Gets entire blockchasin
getBlockchain :: (Monad m) => S.StateT Blockchain m Blockchain
getBlockchain = S.get >>= return


-- StateT Monad to wrap the blockchainaround
restartableStateT :: s -> IO (RunStateT s IO)
restartableStateT s0 = do
    r <- newIORef s0
    return $ RunStateT $ \act -> do
        s <- readIORef r
        (x, s') <- S.runStateT act s
        atomicModifyIORef' r $ const (s', x)


main :: IO ()
main = do
    runner <- restartableStateT [genesisBlock]
    runSpock 8080 $ spockT (runStateT runner) $ do
        get "/" $ (S.lift getBlockchain) >>= json
        get "current" $ (S.lift current) >>= json        
        get "block" $ do
            blockID <- param "blockID"
            case blockID of
              (Just h) -> (S.lift $ getBlockhash h) >>= json
              _        -> text "please supply blockID"
        post "mine" $ (S.lift mine) >>= json
        post "addTransaction" $ do
            sender <- param "sender"
            to <- param "receiver"
            description <- param "description"
            case (sender, to, description) of
              (Just s, Just r, Just d) -> (S.lift $ addTransaction (Transaction s r d)) >>= json
              _                        -> text "missing/incorrect parameters!"