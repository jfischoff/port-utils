# port-utilities

## openFreePort

This is another version of `warp`'s `openFreePort` function. Nice if you don't already depend on `warp`.

`openFreePort` returns a socket on random port and the port it has been bound to.

```haskell
openFreePort :: IO (Int, Socket)
```

## wait

`wait` for attempt to connect to a host and port until successful.

Here is an example of the primary function:

```haskell
import Network.Socket.Wait (wait)

void $ forkIO $ Warp.run 7000 app
-- Wait for the server to start listening on the socket
wait "127.0.0.1" 7000
-- Communicate with the server
...
```

This function will attempt to connect and delay 10 milliseconds and try again until it succeeds.

In bash one could write:

```bash
while ! nc -z localhost 7000 ; do sleep 0.01 ; done
```

The above was copied from this stackoverflow answer https://stackoverflow.com/a/50008755