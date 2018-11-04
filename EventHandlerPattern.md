# The Event Handler Pattern

The `EventHandlers` pattern is a way to write libraries that allow clients to instrument the library code.

The idea is one exposes a simple interface like:

```haskell
foo :: IO ()
```

with an advanced interface

```haskell
fooWith :: EventHandlers -> IO ()
```

that takes in a record of `IO` actions that are called during when important events occur during the execution of `foo`.

It is pretty simple idea but surprisingly useful. I'll walk though the idea in depth by looking a small library I just made called `port-utils`.

## The Design of `port-utils`

`port-utils` is a very simple library. It has one non-trivial function called `wait`. `wait` will attempt to connect to host and port over and over until successful.

Waiting until a server is ready by attempting to connect to it is a useful way to ensure api tests don't fail interminently. The library started as a function I would copy from project to project and eventually bit the bullet and put it up on hackage.

The type signature is:

```haskell
wait :: String -> Int -> IO ()
```

## Gaining Operational Insight

If a client library uses `wait` in production code it is possible they would like to know what is happening inside the loop of `wait`.

Imagine a user knows that their program is stuck in the `wait` function in production but they are not sure why. Ideally `wait` should provide some way to gain insight into why it is blocked. This where the Event Handler pattern comes in.

We offer an advanced API that with the following signature:

```haskell
waitWith :: EventHandlers -> String -> Int -> IO ()
```

The `EventHandlers` is defined as:

```haskell
data EventHandlers = EventHandlers
  { createdSocket :: IO ()
  -- ^ Called after the socket is created
  , delaying      :: IO ()
  -- ^ Called after a failed attempt to connect before the thread
  -- is put to sleep.
  , restarting    :: IO ()
  -- ^ Called before a recursive call to restart the connection attempt
  }
```

The `EventHandlers` let us pass in `IO` actions which are triggered during key moments allowing us to track the progress of `waitWith`. If we passed in `ekg` metrics we could like:

```haskell
createEkgMetrics :: Store -> IO EventHandlers
createEkgMetrics store = do
    createdSocketCounter <- createCounter store "created-socket"
    delayingCounter      <- createCounter store "delaying"
    restartingCounter    <- createCounter store "restarting"

    pure EventHandlers
      { createdSocket = inc createdSocketCounter
      , delaying      = inc delaying
      , restarting    = inc restartingCounter
      }
```

By observing the rate of the metrics we would be able to conclude if `wait` was stuck in the `connect` call (as could happen if the socket was bound to the port but not listening) or if it was failing to connect over and over again.

We could also use the Event Handlers for logging by creating a record like:

```haskell
printEventHandlers :: EventHandlers
printEventHandlers = EventHandlers
  { createdSocket = putStrLn "createdSocket"
  , delaying      = putStrLn "delaying"
  , restarting    = putStrLn "restarting"
  }
```

The `EventHandlers` record is unique to each process but is almost always a `Monoid` so we can combine the two `EventHandlers` records above with `mappend`. Additionally we can define `wait` using `mempty`:

```haskell
wait :: String -> Host -> IO ()
wait = waitWith mempty
```

## Operational Insight Matters

I think operational insight is often more important than establishing correctness. Even if your code is perfect it will still depend on host of other systems that can go wrong. Having the ability to monitor and debug a production system is crucially important regardless of the quality of your code.

The Event Handler pattern allows one to provide the option for operational insight without being directly tied of a logging or metrics library.

The pattern has a few downsides.

If no client library ever uses a non-trivial `EventHandlers` record than you have a provided a feature that complicates the design but buys nothing.

Another downside with the pattern is you are giving your users a footgun they can abuse. If one tries do something complex in the event handler methods you could accidently break an library invariant or just create a very difficult to follow program.

What ever effects occur in the event handlers should be "beign". They should not cause the behavior of the program to change. Various forms of logging should in general work with definition but unconditionally.

On the other hand whatever one does in the handlers will be no worse than making the same call inline.

The primary value for the Event Handler pattern is to provide operational insight without making an arbitrary choice on how to log the events.

It tends to be that once the `EventHandlers` have been setup one can use the record to faciliate testing.

## Conclusion

The `EventHandlers` pattern is simple way to provide client code to gain operational insight into a process without committing to a particular method to ingest the data.
