stack clean
stack build
stack exec -- parallel-sort-exe +RTS -ls -N4
