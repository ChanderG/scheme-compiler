# bash script to run the binary, pass command parameters into it 

# works when using install
# .cabal-sandbox/bin/scheme-compiler $1

#dist/build/scheme-compiler/scheme-compiler "$1" 

read input
dist/build/scheme-compiler/scheme-compiler "$input"


