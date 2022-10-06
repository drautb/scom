type soldier = {
  x : int;
  y : int;
  avatar : char;
  aim : int;
  hp : int;
  def : int;
  actions : int
}

type cover = {
  x : int;
  y : int;
  def : int;
}

type squad = {
  soldiers : soldier list;
}

type command =
| Attack of soldier * soldier
| Move of soldier * int * int
| Cover of soldier

type pending_command =
| None
| Command of command

type world = {
  squads : squad list;
  cover : cover list;
  turn : int;
  pending_cmd : pending_command;
}

type texel = {
  x : int;
  y : int;
  color : ANSITerminal.style list;
  value : char
}

