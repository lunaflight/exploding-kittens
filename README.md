# Exploding Kittens
This is a pet project, implementing the popular game [Exploding
Kittens](https://www.explodingkittens.com) in OCaml.

This project uses `Core` and `Async` heavily and is created to practice the
`Rpc` library.

## Running the project
### Installation
You first need OCaml and [Dune](https://dune.build/install) to run this project.
1. Clone this repository with `git clone`.
2. Run `dune build` to build the project and install the required dependencies.

### Player Command
This command represents the endpoint for a player and interacts with the game
by providing input.

1. Have a process run `dune exec -- ./bin/player.exe -port YOUR_PORT_HERE -name
   YOUR_NAME_HERE`, taking note of the host and port that it is running on. A
   good choice of a port is `8000, 8001, etc`.
2. Repeat the above step as many times for other players.

### Server Command
This command starts the game and is the brain of the game.

1. Start the server by running a process with `dune exec -- ./bin/server.exe
   -player HOST:PORT_1 -player HOST:PORT_2`, replacing the host and port
   accordingly to the players.

## How to play
Watch the [official tutorial video on
YouTube](https://www.youtube.com/watch?v=kAkRKuv5Rts) for the basics of the
game.

Alternatively, read the [official
PDF](https://www.buffalolib.org/sites/default/files/gaming-unplugged/inst/Exploding%20Kittens%20Instructions.pdf)
explaining the rules.

Note: `Favor` and `Nope` are currently unimplemented.
