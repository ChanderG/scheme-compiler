#! /bin/bash

#Chander G
#This is a simple testrunner script to take all files in "etests" folder and run it against the code. It then compares the result with the preset result and prints match/fail.

for ((n = 1;;n++))
do
  if [ -f "etests/t$n.txt" ]
  then 
    ./run.sh < etests/t$n.txt > etests/temp.txt


    # final version : with only yes/no response
    if diff -q etests/temp.txt etests/res$n.txt
    then
      echo "Test #$n passed"
    else
      echo "Test #$n failed:"
      # REMOVE LATER
      #debug mode : full diff between the 2 files
      diff etests/temp.txt etests/res$n.txt
    fi

    rm etests/temp.txt
  else
    echo "Ran " $(($n-1)) " test(s)" 
    break
  fi
done
