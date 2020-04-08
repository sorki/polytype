module SerialSpec where

import SpecHelper

spec :: Spec
spec = return ()

-- not implemented yet, requires e.g. socat loop of two ttys

--test_serial = runM . runResource . res "/dev/tty33" defaultSettings $ readTTY
