# wait-on-port

This is very simple utility library to block excution until a port is ready for connections.

Here is an example of the primary function:

```haskell
import Network.Socket.Wait (wait)

wait "127.0.0.1" 7000
```

This function will attempt to connect and delay 10 milliseconds and try again until it succeeds.

In bash one could write:

```bash
while ! nc -z localhost 7000 ; do sleep 0.01 ; done
```

The above was copied from this stackoverflow answer https://stackoverflow.com/a/50008755