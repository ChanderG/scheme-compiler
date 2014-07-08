#Scheme Compiler in Haskell(WIP)

###What?
My implementation as I follow along the ["Write yourself a Scheme in 48 hours"](http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html). I shall try to follow proper guidelines for development with testing and documentation. The structure will likely change a lot in the starting. 

###Build Instructions
This project uses the [official instructions](http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program) as much as possible.

For now:
```
cabal install -j
.cabal-sandbox/bin/scheme-compiler [args]
```
For generating docs (Haddock):

```
cabal configure
cabal haddock --executables
```

For linting source (Hlint):

```
cabal install hlint
.cabal-sandbox/bin/hlint .
```

For unit testing (HUnit on Cabal):

```
cabal install --enable-tests
cabal test --show-details=streaming
```

###License
MIT
