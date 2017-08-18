module Pipeline.App.AnalysisRunner where



runDaemon = do
  [host, hostB, port, portB] <- getArgs
  pidref              <- newEmptyTMVarIO
  Right transport     <- createTransport host port defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  config <- loadConfig

  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B8.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- loadJVM
    runProcess node $ do
      pid <- spawnLocal $ forever $ do
        (pid :: ProcessId) <- expect
        (query :: Text) <- expect
        liftIO $ print pid
        liftIO $ print query
        result <- liftIO $ fmap (T.intercalate "\n") (getAnalysis query config pp)
        Cloud.send pid result

      liftIO $ do
        atomically (putTMVar pidref pid)
        broadcast pidref portB hostB
