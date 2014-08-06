#Scheme Compiler in Haskell(WIP)

###What?
My implementation as I follow along the ["Write yourself a Scheme in 48 hours"](http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html).The original website is now gone. Luckinly, a copy still remains ["here"](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). 

I shall try to follow proper guidelines for development with testing and documentation. The structure will likely change a lot in the starting. 

###Build Instructions
This project uses the [official instructions](http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program) as much as possible.

For now:

This works when package parameters are required:

```
cabal build
dist/build/scheme-compiler/scheme-compiler [args]
```
while this works only for simple cases. In fact here at this stage of the project it does *not* work at all and is misleading.

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

If HUnit is not installed:
```
cabal install HUnit
```

```
cabal configure --enable-tests
cabal test --show-details=streaming
```

###On the scripts
Three scripts build.sh, run.sh and test.sh are included. They are configured for my own use and *may not* work correctly in your machine.

UPDATE: Sometimes the scripts are not working as they are supposed to. They will be scrapped soon.For now the run script is unsafe to use.

###License
MIT
