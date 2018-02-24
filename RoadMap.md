# rough structure
module Main where
  command-line arguments are parsed (port and whatever)
  session multiplexing (there can be many players and many games)
  menu (create a new game and get the game hash, or join a game)
  there's no client, you just telnet

module Game where
  formally we are interested in non-cooperative symmetric zero-sum sequential games with perfect information
  we have variants for 2 and 3 players
  number of players uniquely determines the grid (3x3, square or hex)
  we have moves which can be valid or not (depends on a particular game state)
  moves are also classic or quantum
  we have game state (finished with a winner or a draw, or running with a current player, or not yet started/waiting for players)
