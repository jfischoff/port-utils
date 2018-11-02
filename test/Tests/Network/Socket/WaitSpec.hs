module Tests.Network.Socket.WaitSpec where
import qualified Network.Socket.Wait as W
import qualified Network.Socket as N
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Exception as E
import qualified Test.Hspec as H
import qualified System.Timeout as T

main :: IO ()
main = H.hspec spec

spec :: H.Spec
spec = H.before W.openFreePort $ H.after (N.close . snd) $ H.describe "wait" $ do
  H.it "wait retries if a socket is not ready" $ \(port, sock) -> do
    -- There is a small race some what might connect to this
    N.close sock

    retryRef <- S.newTVarIO 0

    let retryCount = 2

        handlers = mempty { W.restarting = S.atomically $ S.modifyTVar' retryRef (+1) }

    let start = W.waitWith handlers W.defaultDelay "127.0.0.1" port
    E.bracket (C.forkIOWithUnmask $ \restore -> restore start) C.killThread $ \_ ->
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
