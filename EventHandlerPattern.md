# The Event Handler Pattern

The Event Handler pattern is a way to write libraries that allow clients to instrument the library code.

The idea is one exposes a simple interface like `foo :: IO ()` with an advanced interface `fooWith :: EventHandlers -> IO ()` that takes in a record of `IO` actions that are called during when important events occur during the execution of `foo`.

It is pretty simple idea but surprisingly useful. I'll walk though the idea in depth by looking a small library I just made called `port-utils`.

## The Design of `port-utils`

`port-utils` is a very simple library. It has one non-trivial function called `wait`. `wait` will attempt to connect to host and port over and over until successful.

Waiting until a server is ready by attempting to connect to it is a useful way to ensure api tests don't fail interminently. The library started as a function I would copy from project to project and eventually bit the bullet and put it up on hackage.

The type signature is:

```haskell
wait :: String -> Int -> IO ()
```

So how does one test a function like `wait` which is basically `IO ()`? The second a function is in `IO` it is a more of a pain to test and functions that eat data and return nothing are more annoying.

I came up with three success criteria:
1. If there is no port availble to connect to `wait` should loop forever.
1. If the port is available before it starts it should immediantly connect.
1. If the port is initially unavailable `wait` should retry until it is available.

### Black box testing `wait`

#### Black Box Testing with Timeouts

Black box testing a function returns `IO ()` is annoying. We _can_ black box test `wait` using timeouts.

Ensuring that `wait` loops forever is simple enough, we just need to start `wait` and ensure it blocks until some amount of time we deem is "enough". Crucially even if our timeout mechanism is unreliable the test will not fail intermetiantly. Regardless of the time limit `wait`s behavior is the same.

Ensuring that `wait` connects is nearly as simple. First we must ensure a port is in a listening state. Next we should call `wait` and give it a very large timeout for connecting to an address. I bet it takes in single milliseconds so if our timeout is in the hundreds of milliseconds we should be okay.

Ensuring `wait` succeeds after initially failing is trickier. First we need to ensure that `wait` has been running long enough to fail to connect. We don't have a way to detect this from the `wait` function directly. We can try to come up with timeouts we believe will `wait` enough to fail at least once.

The timeout approach is weak because reliability is directly portional to the length of the timeouts and thus the test. If you want get many nine's of reliability you might need to make the timeouts orders of magnitude greater than the

The other problem with timeout approachs is although they are able to verify that `wait` meets the most important success criteria, they can't ensure that `wait` is functioning optimally. Ideally waiting should attempt the fewest connections necessary but we have not way to.

The main advantage to this approach, which is the advantage of black box testing in general, the test is dependent on the internals of the implementation.

#### Grey Box Testing through Traffic Analysis

Because we know general socket calls that `wait` makes we can observe the behavior of `wait` using the OS.

`wait` emits TCP/IP packets when it calls `connect`. We can observe this traffic.

If a successful connect occurs we can observe a `SYN` -> `SYN/ACK` -> `ACK` TCP handshake flow.

If a connection fails we should see a `SYN` -> `RST` packet flow.

We can use our ability to detect these events to more promptly move through the steps necessary complete the final test.

Another way to think about is we need to make every run of test three with the timeout the length of the worse case. Using our packet detecting funcions we still have the same worse case but are typical case will be orders of magnitude faster.

The downside to this approach is our test is more complicated and inimately dependent on the OS. We have a faster more reliable test but maintaining it will be more costly.

Additionally we are assuming something about the internal structure of wait. However I am okay depending on TCP working the same in the forseeable future.

#### White Box Testing Event Handler Pattern

We can't white box test `wait`. There is no way monitor the internal state. To allow white box testing we need to make a new function `waitWith` with the following signature:

```haskell
waitWith :: EventHandlers -> String -> Int -> IO ()
```

and we redefine `wait` to be

```haskell
wait = waitWith mempty
```

We can then whitebox test `waitWith` and as long as passing `mempty` doesn't invalidate the tests we can basked confidence of knowing `wait` has been validated.

We can't talk in generalities about white box testing. It is by nature highly dependent on the structure of the code. One goal is make sure all the events like connection attempts and retries are observable.

Let's start by writing `wait` and refactoring it to `waitWith`. Then we can write our tests.

#### Deconstructing the Loop

When we test `wait`s behavior we are testing the control flow of the loop and the behavior of calls like `connect`. We could instead create a function `waitM :: m Bool -> m ()` and verify that it loops as expected. Then we make a `catchAndConnect` function and we are good.

* TODO you and use this function
