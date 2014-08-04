# builds test suite and runs it

~/.cabal/bin/cabal configure --enable-tests
~/.cabal/bin/cabal test --show-details=streaming $1


