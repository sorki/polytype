multitype
==========

Blah

Usage
-----

```haskell
import qualified Data.Cayene as C

main :: IO ()
main = do
  let x = (0, AnalogIn 4.48)
  print $ C.decode $ C.encode x
```
