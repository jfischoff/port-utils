module Tests.Network.Socket.WaitSpec where
import qualified Network.Socket.Wait as W
import qualified Network.Socket.Free as F
import qualified Network.Socket as N
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Exception as E
import qualified Test.Hspec as H
import qualified System.Timeout as T
import qualified Control.Concurrent.Async as A

main :: IO ()
main = H.hspec spec

data WaitState
  = Start
  | Retried
  | Open
  deriving (Show, Eq)

throwTimeout :: Int -> String -> IO a -> IO a
throwTimeout delay msg action = T.timeout delay action >>= \case
  Nothing -> fail $ "timed out " ++ msg
  Just x -> pure x

spec :: H.Spec
spec = H.before F.openFreePort $ H.after (N.close . snd) $ H.describe "wait" $ do
  H.it "retries if a socket is not ready" $ \(port, sock) -> do
    -- There is a small race some what might connect to this
    N.close sock

    retryRef <- S.newTVarIO 0

    let retryCount = 2

        handlers = mempty { W.restarting = S.atomically $ S.modifyTVar' retryRef (+1) }

    let start = W.waitWith handlers W.defaultDelay "127.0.0.1" port
    E.bracket (C.forkIO start) C.killThread $ \_ ->
      T.timeout
        (retryCount * W.defaultDelay * 10) -- wait more than long enough
        (S.atomically $ S.check . (==2) =<< S.readTVar retryRef) >>= \case
          Nothing
            -> fail
            $ "timed out waiting for the wait to retry "
            ++ show retryCount
            ++ " times"
          Just _  -> pure ()

  H.it "connects immediantly to socket when it is ready" $ \(port, sock) -> do
    N.listen sock 128

    retryRef <- S.newTVarIO 0

    let handlers = mempty { W.restarting = S.atomically $ S.modifyTVar' retryRef (+1) }

    T.timeout
      (W.defaultDelay * 10) -- wait more than long enough
      (W.waitWith handlers W.defaultDelay "127.0.0.1" port) >>= \case
        Nothing -> fail "timed out waiting to connect"
        Just _  -> S.atomically (S.readTVar retryRef) `H.shouldReturn` 0

  H.it "connects even if the socket is not open at first" $ \(port, sock) -> do
    N.close sock
    retryRef <- S.newTVarIO Start

    let handlers = mempty
          { W.restarting    = S.atomically $ S.writeTVar retryRef Retried
          , W.createdSocket = S.atomically $ S.readTVar retryRef >>= \case
              -- Pass through and first and fail
              Start   -> pure ()
              -- Block until listening
              Retried -> S.retry
              -- Pass through
              Open    -> pure ()
          }

    let start = W.waitWith handlers W.defaultDelay "127.0.0.1" port
    throwTimeout 100000 "waiting for wait to return"
      $ E.bracket (A.async start) A.wait $ \_ -> do
        S.atomically $ S.check . (==Retried) =<< S.readTVar retryRef

        E.bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close
          $ \sock' -> do
            S.atomically $ S.writeTVar retryRef Open
            N.bind sock' $ N.SockAddrInet (fromIntegral port) $ N.tupleToHostAddress (127,0,0,1)
            N.listen sock' 128
