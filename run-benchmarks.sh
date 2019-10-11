stack clean
stack build
stack exec -- parallel-sort-run-benchmarks --output benchmarks.html +RTS -ls
