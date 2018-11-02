module Network.Socket.Wait
  ( -- * Simple Wait Api
    wait
    -- * Advanced Wait Api
  , waitWith
  , EventHandlers (..)
  , defaultDelay
  ) where
import qualified Network.Socket as N
import qualified Control.Concurrent as C
import qualified Control.Exception as E

-------------------------------------------------------------------------------
-- Simple Api
-------------------------------------------------------------------------------

-- | 'wait' will attempt to connect to the given host and port repeated every
-- 10 milliseconds until it is successful. It will throw an 'IOError' if the
-- host cannot be resolved.
--
-- A typical use case is to call 'wait' in test code to wait for a server to
-- start before trying to connect. For example:
--
-- @
--    void $ forkIO $ Warp.run 7000 app
--    -- Wait for the server to start listening on the socket
--    wait "127.0.0.1" 7000
--    -- Communicate with the server
-- @
--
-- If you would like to control the delay or understand how many connection
-- attempts were made use 'waitWith'.
--
-- Since 0.0.0.1
wait :: String
     -- ^ Host
     -> Int
     -- ^ Port
     -> IO ()
wait = waitWith mempty defaultDelay

-------------------------------------------------------------------------------
-- Advanced Api
-------------------------------------------------------------------------------
-- | The default delay between retries is 10000 microseconds (10 ms)
defaultDelay :: Int
defaultDelay = 10000

-- | The 'EventHandlers' is a record of 'IO' actions that are called when
--   interesting events occur in the lifecycle of the 'waitWith' loop.
--   One can pass in custom 'EventHandlers' values to implement logging
--   and other forms of instrumentation.
--
--   For example for debug one could print out each step using:
--
-- @
--    printHandlers = EventHandlers
--      { createdSocket = putStrLn "createdSocket"
--      , delaying      = putStrLn "delaying"
--      , restarting    = putStrLn "restarting"
--      }
-- @
--
data EventHandlers = EventHandlers
  { createdSocket :: IO ()
  -- ^ Called after the socket is created
  , delaying      :: IO ()
  -- ^ Called after a failed attempt to connect before the thread
  -- is put to sleep.
  , restarting    :: IO ()
  -- ^ Called before a recursive call to restart the connection attempt
  }

instance Semigroup EventHandlers where
  x <> y = EventHandlers
    { createdSocket = createdSocket x <> createdSocket y
    , delaying      = delaying      x <> delaying      y
    , restarting    = restarting    x <> restarting    y
    }

instance Monoid EventHandlers where
  mempty  = EventHandlers mempty mempty mempty
  mappend = (<>)

-- | Advanced usage. In most situations calling 'wait' will suffice. This allows
-- one to customize the delay between retries and debug the behavior of the
-- function. 'wait' is defined as
--
-- @
--    wait = waitWith mempty defaultDelay
-- @
--
-- Since 0.0.0.1
waitWith :: EventHandlers
         -- ^ A record of IO actions that are called during sp
         -> Int
         -- ^ Microseconds to delay
         -> String
         -- ^ Host
         -> Int
         -- ^ Port
         -> IO ()
waitWith eh@EventHandlers {..} delay host port = do
  res <- E.try $ do
    let hints = N.defaultHints { N.addrSocketType = N.Stream }
    -- getAddrInfo returns a non-empty array or throws per the doc
    addr:_ <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)
    E.bracket
      (N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr))
      N.close
      $ \sock -> do
        createdSocket
        N.connect sock $ N.addrAddress addr

  case res of
    Left (_ :: E.IOException) -> do
      delaying
      C.threadDelay delay

      restarting
      waitWith eh delay host port
    Right _ -> pure ()
