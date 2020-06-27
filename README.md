# polytype

**🚧 Work in progress 🚧**

## What is this?

* Polymorphic [Teletype][teletype]
* Library for interacting with
  * programs
  * programs running in POXIS pseudo terminals via [posix-pty][hackage:posix-pty]
  * serial terminals via [serialport][hackage:serialport]
* Expect on steroids
* Program interaction library with [Asciinema][asciinema] recorder
* My first [polysemy] endeavour and a playground (for now)
* Collection of useful effects related to the topic

## What is it good for?

* Interacting with and testing other programs
* Testing of terminal UIs
* Making demos of terminal UIs with [Asciinema][asciinema] [asciicast][asciicast] output
* Testing hardware (or virtual machines) over serial connection
* Streaming inputs and outputs of programs, terminals, serial connections
* Variations of the previous ones

## Why?

To explore effect systems and polymorphic IO automation framework. Originally the project
was called `multitype`, based on `Free` monads which proved difficult to interpret with
all the required bits like `async` and `timeout`.
Later it was rewritten using `transformers` but never released because it still felt too
opinionated about the use of e.g. character vs line based input or `Text` vs `ByteString`
specialization.

This library tries to be much less picky about the type of its inputs and outputs
allowing the user to choose what types to work with or convert between different
[Teletypes](src/Polytype/Teletype.hs).
[polysemy]s effect system allows the user to write programs using
the same eDSL and choosing interpreters according to the target environment.

## Examples

```haskell
import Polytype

main :: IO ()
main =
  . runM
  . teletypeToIO
  . runLogShow
  . teletypeLog
  $ do
    writeTTY "Type something"
    i <- readTTY
    writeTTY $ "You wrote: " ++ i
```

### Provided examples

* [bash](src/Polytype/Examples/Bash.hs)
* [echo](src/Polytype/Examples/Echo.hs)
* [htop demo](src/Polytype/Examples/HtopAsciinema.hs)
* [ircbridge test](src/Polytype/Examples/IRCBridge.hs)
* [process](src/Polytype/Examples/Process.hs)
* [safe process](src/Polytype/Examples/SafeProcess.hs)
* [streaming](src/Polytype/Examples/Streaming.hs)

### Asciinema output

[![asciicast](https://asciinema.org/a/343380.svg)](https://asciinema.org/a/343380)

## Development status

Types of effects are in pretty good shape. Names and interpreters are subject to change.

The library grew quite a bit since the work started and some parts will be split into
sub-packages.

## Related work

Inspired by:

* [teletype]
* [polysemy]
* [polysemys Teletype example][hackage:polysemy]
* [co-log]s polymorphic logger effect
* and few other bits credited in source files

[co-log]: https://github.com/kowainik/co-log/
[polysemy]: https://github.com/polysemy-research/polysemy
[hackage:polysemy]: https://github.com/polysemy-research/polysemy
[hackage:serialport]: https://hackage.haskell.org/package/serialport
[hackage:posix-pty]: https://hackage.haskell.org/package/posix-pty
[teletype]: http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html
[asciinema]: https://asciinema.org/
[asciicast]: https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md
