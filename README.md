#Scheme Compiler in Haskell(WIP)

###What?
My implementation as I follow along the ["Write yourself a Scheme in 48 hours"](http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html).The original website is now gone. Luckily, a copy still remains ["here"](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). 

I shall try to follow proper guidelines for development with testing and documentation. The structure will likely change a lot in the starting. 

Right now, on the first round I shall mainly stick to the given tutorial. The exercises will be implemented in a later iteration.

**UPDATE: The testing etc has been done properly till before the REPL. I cannot say that I understand everything after this point.Hence I am blindly sticking to the writing as of now.**


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

The test.sh runs the HUnit tests.

UPDATE: Sometimes the scripts are not working as they are supposed to. They will be scrapped soon.For now the run script is unsafe to use.

UPDATE to the UPDATE: The error has been corrected. The scripts should be safe now.

Double quotes in the run script arguments needs to be escaped.

The run script is now just a short cut to run the binary.

###External tester

The etest.sh is an external test runner. It runs all t$n.txt files from the etests folder and compares the results produced with the expected pre-calculatd results in res$n.txt.

```
./etest.sh
```

This does not test the REPL as of now.

###License
MIT
