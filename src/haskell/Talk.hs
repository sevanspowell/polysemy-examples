{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Talk where

import Data.Function ((&))
import Data.Void (Void)
import Polysemy
import Polysemy.Output
import Polysemy.Input
import Polysemy.State
import Polysemy.Trace

import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitT, (.|), runConduitRes)
import qualified Data.Conduit.Combinators as C
import Control.Monad.Trans.Resource (MonadResource, runResourceT, ResourceT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as T

data Stat = ProcessedRecordStat

data Record = Record B.ByteString
  deriving Show

instance A.ToJSON Record where
  toJSON (Record bs) = A.String (T.decodeUtf8 bs)

data FileProvider m a where
  OpenFile :: FilePath -> FileProvider m B.ByteString

makeSem ''FileProvider

data Encryption m a where
  DecryptStream :: B.ByteString -> Encryption m B.ByteString

makeSem ''Encryption

data POST

data HttpRequest (method :: *) where
  HttpRequest :: B.ByteString -> HttpRequest method

data HTTP m a where
  PostHttp :: HttpRequest POST -> HTTP m ()

makeSem ''HTTP

localFileProvider
  :: Member (Lift IO) r
  => Sem (FileProvider ': r) a
  -> Sem r a
localFileProvider = interpret $ \case
  OpenFile fp -> sendM $ B.readFile fp

csvInput
  :: ( Member FileProvider r
     )
  => FilePath
  -> Sem (Input (Maybe Record) ': r) a
  -> Sem r a
csvInput source m = do
  stream <- openFile source

  let streamLines = Record <$> B.lines stream

  -- runListInput runs the input effect by providing a new element of
  -- the list to each input effect.
  -- TODO investigate possibility of conduit with runMonadicInput
  runListInput streamLines m

decryptFileProvider
  :: Member Encryption r
  => Sem (FileProvider ': r) a
  -> Sem (FileProvider ': r) a
decryptFileProvider =
  -- Insert some logic around the 'FileProvider' effect so that we
  -- make sure to decrypt the file (using the 'Encryption' effect)
  -- before reading it.
  -- We need to use 'reinterpret2' because we are introducing a new
  -- effect and persisting the old one.
  intercept $ \case
    OpenFile file   -> do
      stream <- openFile file
      decryptStream stream

runEncryption :: Sem (Encryption ': r) a -> Sem r a
runEncryption = interpret $ \case
  DecryptStream stream -> pure $ stream

batch
  :: ∀ o r a
   . Int
  -> Sem (Output o ': r) a
  -> Sem (Output [o] ': r) a
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
                                                n | n == batchSize -> do
                                                      output acc
                                                      put [o]
                                                  | otherwise -> put (acc <> [o])
                                                        
                                                  ) m
  when (length leftOver > 0) $ output leftOver
  pure a

postOutput
  :: ( A.ToJSON i
     , Member HTTP r
     )
  => (i -> HttpRequest POST)
  -> Sem (Output i ': r) a
  -> Sem r a
postOutput mkReq = interpret $ \case
  Output i -> postHttp $ mkReq i

mkApiCall :: A.ToJSON i => i -> HttpRequest POST
mkApiCall = HttpRequest . BL.toStrict . A.encode

runHTTP :: Member (Lift IO) r => Sem (HTTP ': r) a -> Sem r a
runHTTP = interpret (\case
  PostHttp (HttpRequest body) -> sendM . putStrLn . show $ body
                    )
  
ingest
  :: ( Member (Input (Maybe Record)) r
     , Member (Output Record) r
     -- , Member (Output Stat) r
     )
  => Sem r ()
ingest = input @(Maybe Record) >>= \case
  Nothing     -> pure ()
  Just record -> do
    output @Record record
    -- output ProcessedRecordStat
    ingest

mainP :: IO ()
mainP = ingest
     & csvInput "file.csv"
     & decryptFileProvider
     & localFileProvider
     & batch @Record 2
     & postOutput @[Record] mkApiCall
     & runEncryption
     & runHTTP
     & runM
        

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
