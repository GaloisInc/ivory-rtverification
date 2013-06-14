# BSD3
# Lee Pike
# (c) 2013 Galois, Inc.

# Finds all .c files, looks for instrument attributes
#
# int foo __attribute__ ((instrument(0)));
# char bar __attribute__ ((instrument(1)));
#
# Returns those lines

#!/bin/sh

# XXX pattern match out spaces
function findLns {
  find $1 -name "*.c" | xargs grep "__attribute__((instrument" --no-filename
}

function run {
    if [ "$1" = "" ];
    then findLns .
    else findLns $1
    fi
}

run $1
