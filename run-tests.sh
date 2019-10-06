stack clean
stack build
stack exec -- parallel-sort-run-tests +RTS -ls -N4
