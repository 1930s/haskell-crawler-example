### Wikipedia page crawler in Haskell

Web page crawler implemented in Haskell.

#### Build and run

Output will be placed into `output` directory.

```bash
$ stack build
$ stack exec crawler-exe -- -c1 -l50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"
```

#### Sample runs

```bash
$ time stack exec crawler-exe -- +RTS -N1 -RTS -c1 -l50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"
Caching build plan
start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
concurrency level: 1, visited link limit: 50
Done. 51 links visited.
stack exec crawler-exe -- +RTS -N1 -RTS -c1 -l50   11.17s user 1.26s system 49% cpu 24.988 total
$ time stack exec crawler-exe -- +RTS -N1 -RTS -c5 -l50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"
start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
concurrency level: 5, visited link limit: 50
Done. 51 links visited.
stack exec crawler-exe -- +RTS -N1 -RTS -c5 -l50   6.36s user 1.42s system 94% cpu 8.273 total
$ time stack exec crawler-exe -- +RTS -N1 -RTS -c10 -l50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"
start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
concurrency level: 10, visited link limit: 50
Done. 51 links visited.
stack exec crawler-exe -- +RTS -N1 -RTS -c10 -l50   6.89s user 1.31s system 145% cpu 5.644 total
$ time stack exec crawler-exe -- +RTS -N2 -RTS -c1 -l50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"                                                <<<
start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
concurrency level: 1, visited link limit: 50
Done. 51 links visited.
stack exec crawler-exe -- +RTS -N2 -RTS -c1 -l50   6.04s user 1.29s system 55% cpu 13.092 total
$ time stack exec crawler-exe -- +RTS -N2 -RTS -c5 -l50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"
start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
concurrency level: 5, visited link limit: 50
Done. 51 links visited.
stack exec crawler-exe -- +RTS -N2 -RTS -c5 -l50   7.02s user 1.36s system 139% cpu 6.010 total
$ time stack exec crawler-exe -- +RTS -N2 -RTS -c10 -l50 "https://en.wikipedia.org/wiki/Alexander_Pushkin"
start crawling from https://en.wikipedia.org/wiki/Alexander_Pushkin
concurrency level: 10, visited link limit: 50
Done. 51 links visited.
stack exec crawler-exe -- +RTS -N2 -RTS -c10 -l50   7.98s user 1.48s system 185% cpu 5.100 total
```
