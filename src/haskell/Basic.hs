{-# LANGUAGE TemplateHaskell #-}

module Basic where

import Polysemy

data Console m a where
  ReadTTY  :: Console m String
  WriteTTY :: String -> Console m ()

makeSem ''Console

runConsoleIO :: Member (Lift IO) r => Sem (Console ': r) a -> Sem r a
runConsoleIO = interpret $ \case
  ReadTTY      -> sendM getLine
  WriteTTY msg -> sendM $ putStrLn msg

ioRunner :: Sem [Console, Lift IO] a -> IO a
ioRunner = runM . runConsoleIO

helloWorld :: IO ()
helloWorld = ioRunner $ writeTTY "Hello world"

simpleEcho :: Sem (Console ': r) ()
simpleEcho = do
  input <- readTTY
  writeTTY (input <> " REDACTED")
