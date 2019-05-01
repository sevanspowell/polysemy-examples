{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Talk where

import Data.Function ((&))
import Data.Void (Void)
import Polysemy
import Polysemy.Output
import Polysemy.Input
import Polysemy.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitT, (.|), runConduitRes)
import qualified Data.Conduit.Combinators as C
import Control.Monad.Trans.Resource (MonadResource, runResourceT, ResourceT)
import Control.Monad.IO.Class (liftIO)

data Stat = ProcessedRecordStat
data Record = Record B.ByteString

data FileProvider m a where
  OpenFile :: FilePath -> FileProvider m B.ByteString

makeSem ''FileProvider

data Encryption m a where
  DecryptFile :: FilePath -> Encryption m a

makeSem ''Encryption

localFileProvider
  :: Member (Lift IO) r
  => Sem (FileProvider ': r) a
  -> Sem r a
localFileProvider = interpret $ \case
  OpenFile fp -> sendM @IO $ B.readFile fp

csvInput :: forall i r a s . FilePath -> Sem (Input (Maybe Record) ': r) a -> Sem (FileProvider ': r) a
csvInput source m = do
  stream <- openFile source

  let streamLines = Record <$> B.lines stream

  -- runListInput runs the input effect by providing a new element of
  -- the list to each input effect.
  -- TODO look at runMonadicInput
  raise $ runListInput streamLines m

decryptFileProvider :: Sem (FileProvider ': r) a -> Sem (Encryption ': FileProvider ': r) a
decryptFileProvider =
  -- Insert some logic around the 'FileProvider' effect so that we
  -- make sure to decrypt the file (using the 'Encryption' effect)
  -- before reading it.
  -- We need to use 'reinterpret2' because we are introducing a new
  -- effect and persisting the old one.
  reinterpret2 $ \case
    OpenFile file   -> decryptFile file

batch :: ∀ o r a. Int -> Sem (Output o ': r) a -> Sem (Output [o] ': r) a
batch 0 m         =
  -- If batch size is 0, we:
  -- 'runIgnoringOutput' which will remove the output effects from the
  -- effect list. Unfortunately, this gives us a resulting type
  -- 'Sem r a', and we need "Sem (Output [o] ': r) a".
  -- This is what 'raise' is for, it lets us introduce any arbitrary
  -- effect into our effect list:
  --   raise :: ∀ e r a. Sem r a -> Sem (e ': r) a
  raise $ runIgnoringOutput m
batch batchSize m = do
  -- Now for the interesting case.
  -- The basic run down is that we're going to temporarily introduce a
  -- new state effect to group the outputs into batches.

  -- We consume each 'Output' effect and add it's object to a list
  -- kept in the 'State' effect. When that list reaches the batchSize,
  -- we issue an output effect with the batch. When we've handled all
  -- the 'Output' effects, we make sure to output the objects
  -- leftover.
  -- 
  -- This function provides an interesting insight into how
  -- reinterpret works. We're providing a natural transformation that
  -- runs over /all/ the effects in the stack, we're processing them
  -- all here, not one at a time. Up to this point I had a
  -- 'one at a time' mental model. I've been thinking of it now as
  -- more of a 'fold', collapsing all the effects and producing a new
  -- set.
  --
  -- We use 'reinterpret2' to add the state effect, allowing us to
  -- accumulate the output effects, and 'runState' to remove the state
  -- effect.
  (leftOver, a) <-
    runState ([] :: [o]) $ reinterpret2 (\case
                                            Output o -> do
                                              acc <- get
                                              case length acc of
                                                n | n >= batchSize -> do
                                                      let (emit, acc') = splitAt batchSize acc
                                                      output emit
                                                      put acc'
                                                  | otherwise -> put (acc <> [o])
                                                        
                                                  ) m
  output leftOver
  pure a

ingest
  :: ( Member (Input (Maybe Record)) r
     , Member (Output Record) r
     , Member (Output Stat) r
     )
  => Sem r ()
ingest = input >>= \case
  Nothing     -> pure ()
  Just record -> do
    output @Record record
    output ProcessedRecordStat
    ingest

-- main = ingest
--      & csvInput "file.csv"
--      & decryptFileProvider
--      & localFileProvider
--      & batch @Record 500

-- main = ingest
--        -- { Input Record, Output Record, Output Stat }
--        & csvInput "file.csv"
--        -- { FileProvider, Output Record, Output Stat }
--        & decryptFileProvider
--        -- { FileProvider, Output Record, Output Stat, Encryption }
--        & ftpFileProvider
--        -- { FTP, Output Record, Output Stat, Encryption }
--        & batch @Record 500
--        -- { FTP, Output [Record], Output Stat, Encryption }
--        & postOutput @Record mkApiCall
--        -- { FTP, HTTP, Output Stat, Encryption }
--        & redisOutput @Stat mkRedisKey
--        -- { FTP, HTTP, Redis, Encryption }
--        & runEncryption
--        -- { FTP, HTTP, Redis }
--        & runHTTP
--        -- { FTP, Redis }
--        & runFTP
--        -- { Redis }
--        & runRedis
--        -- { }
--        & runM
