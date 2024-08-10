# Using Dune

You first need OCaml and [Dune](https://dune.build/install) to run this project.
1. Clone this repository with `git clone`.
2. Run `dune build` to build the project and install the required dependencies.

# How to run
1. Have a process run `dune exec -- ./bin/player.exe -port YOUR_PORT_HERE`,
   taking note of the host and port that it is running on. A good choice of a
   port is `8000, 8001, etc`.
2. Repeat the above process for another player.
3. Start the server by running a process with `dune exec -- ./bin/server.exe
   -player HOST:PORT_1 -player HOST:PORT_2`, replacing the host and port
   appropriately.

# Commands for development
- To build the project: `dune build`
- To run the formatter: `dune fmt`
- To run tests and accept changes: `dune runtest --auto-promote`
- To run it while watching changes to the directory, consider running
`dune build @fmt @runtest --auto-promote --watch`.
