{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Network.Socket.WaitSpec where
import qualified Network.Socket.Wait.Internal as W
import qualified Network.Socket.Free as F
import qualified Network.Socket as N
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as S
import qualified Control.Exception as E
import qualified Test.Hspec as H
import qualified System.Timeout as T
import qualified Control.Concurrent.Async as A
import qualified Control.Monad.Trans.State as St

instance Monad m => Semigroup (St.StateT s m ()) where
  (<>) = (>>)

instance Monad m => Monoid (St.StateT s m ()) where
  mempty = pure ()
  mappend = (<>)

main :: IO ()
main = H.hspec spec

spec :: H.Spec
spec = do
  H.describe "wait" $ H.before F.openFreePort $ H.after (N.close . snd) $ do
    H.it "does not connect if port is unavailable" $ \_ -> do
      T.timeout 100000 (W.wait "127.0.0.1" 0) >>= \case
        Nothing -> pure ()
        Just () -> fail "wait returned! I should have blocked forever!"

    H.it "does connect if port is available" $ \(port, sock) -> do
      N.listen sock 128

      T.timeout 100000 (W.wait "127.0.0.1" port) >>= \case
        Nothing -> fail "wait timed out attempting to connect! I should have returned immediantly!"
        Just () -> pure ()

  H.describe "connectAction" $ H.before F.openFreePort $ H.after (N.close . snd) $ do
    H.it "returns False if it fails to connect" $ \_ ->
      W.connectAction "127.0.0.1" 0 `H.shouldReturn` False

    H.it "returns True if it connects" $ \(port, sock) -> do
      N.listen sock 128
      W.connectAction "127.0.0.1" port `H.shouldReturn` True

    H.it "returns True if connects after failing" $ \(port, sock) -> do
      N.close sock
      -- small race here if another process opens on this port

      W.connectAction "127.0.0.1" port `H.shouldReturn` False

      E.bracket (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \sock' -> do
         N.bind sock' $ N.SockAddrInet (fromIntegral port) $ N.tupleToHostAddress (127,0,0,1)
         N.listen sock' 128

         W.connectAction "127.0.0.1" port `H.shouldReturn` True

    H.it "throws if the host does not exist" $ \(_, _) -> do
      W.connectAction "invalid." 3000 `H.shouldThrow` (\(_ :: IOError) -> True)

  H.describe "waitM" $ do
    H.it "returns immediantly if the action returns True" $
      flip St.execState False (W.waitM mempty (St.put True) (pure True)) `H.shouldBe` False

    H.it "loops until the action returns True" $ do
      let theAction = do
            St.get >>= \case
              0 -> St.put 1 >> pure False
              _ -> St.put 2 >> pure True

      flip St.execState 0 (W.waitM mempty (pure ()) theAction) `H.shouldBe` 2
