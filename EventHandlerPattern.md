# The Event Handler Pattern

The `EventHandlers` pattern is a way to write libraries clients can instrument.

If you have a simple interface like

```haskell
foo :: Context ()
```

You can also expose an advanced interface

```haskell
fooWith :: EventHandlers -> Context ()
```

where `EventHandlers` is a record of `Context` actions that are called at key moments inside `fooWith`.

```haskell
data EventHandlers = EventHandlers
  { beforeMissleLaunch :: Context ()
  , fundingSecured     :: Int -> Context ()
  }
```

A simple library I made called [`port-utils`](http://hackage.haskell.org/package/port-utils) highlights this pattern.

## The Design of `port-utils`

[`port-utils`](http://hackage.haskell.org/package/port-utils) is a very simple library. It has one non-trivial function called `wait`. `wait` will attempt to connect to host and port over and over until successful.

The type signature of `wait` is:

```haskell
wait :: String -> Int -> IO ()
```

## Gaining Operational Insight

When a user calls `wait` it is possible they would like to know what is happening internally.

Ideally `wait` would provide some way to explain what it is doing. This is where the `EventHandler` pattern comes in.

We offer an advanced API that with the following signature:

```haskell
waitWith :: EventHandlers -> String -> Int -> IO ()
```

The `EventHandlers` is defined as:

```haskell
data EventHandlers = EventHandlers
  { connecting :: IO ()
  -- ^ Called before the socket is created
  , delaying :: IO ()
  -- ^ Called after a failed attempt to connect before the thread
  -- is put to sleep.
  , restarting :: IO ()
  -- ^ Called before a recursive call to restart the connection attempt
  }
```

The `IO` actions are triggered during key moments allowing one to track the progress of `waitWith`. If we passed in `ekg` metrics:

```haskell
createEkgMetrics :: Store -> IO EventHandlers
createEkgMetrics store = do
    connectingCounter <- createCounter store "connecting"
    delayingCounter   <- createCounter store "delaying"
    restartingCounter <- createCounter store "restarting"

    pure EventHandlers
      { createdSocket = inc connectingCounter
      , delaying      = inc delaying
      , restarting    = inc restartingCounter
      }
```

we could observe the rate of the counter and conclude if `wait` was stuck in the `connect` call or if it was failing to connect over and over again.

We could also use the `EventHandlers` for logging by creating a record like:

```haskell
printEventHandlers :: EventHandlers
printEventHandlers = EventHandlers
  { connecting = putStrLn "connecting"
  , delaying   = putStrLn "delaying"
  , restarting = putStrLn "restarting"
  }
```

The `EventHandlers` record is unique to each process but is almost always a `Monoid` since each member is `Monoid`.

```haskell
instance Semigroup EventHandlers where
  x <> y = EventHandlers
    { connecting = connecting x <> connecting y
    , delaying   = delaying   x <> delaying   y
    , restarting = restarting x <> restarting y
    }

instance Monoid EventHandlers where
  mempty  = EventHandlers mempty mempty mempty
  mappend = (<>)
```

Then we can define `wait` using `mempty`:

```haskell
wait :: String -> Host -> IO ()
wait = waitWith mempty
```

## Operational Insight Matters

Even if you code is correct your applications can fail because of dependencies outside your control.

The `EventHandlers` pattern allows one to provide the option for operational observability without being directly tied to a logging or metrics library.

The pattern has a few downsides.

1. It is more complicated than doing nothing.
1. Not practical for tight loops.
1. It's a footgun. A user can do evil things and break library invariants.
1. It too simple for instrumentation that requires a call graph of the events.

That said, I find the power to weight ratio to be high.

## Conclusion

The `EventHandlers` pattern is a simple way for users of your library to gain operational insight without making an arbitrary choice on how to log the events.
