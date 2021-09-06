[![Build](https://github.com/mgrojo/play_2048/actions/workflows/main.yml/badge.svg)](https://github.com/mgrojo/play_2048/actions/workflows/main.yml)

This is yet another implementation of the [2048
game](https://github.com/gabrielecirulli/2048), implemented in Ada
using [ASFML](https://mgrojo.github.io/ASFML/) for graphics and
[ada-toml](https://github.com/pmderodat/ada-toml) for saving state.

It has been tested under Windows 10 and Ubuntu Linux 20.04.

# Features

- Different [themes](/themes/) can be switched while playing. Five themes are
included, but the user can add up to nine themes, following the same
structure.
- Best score and best time
- Fullscreen
- Saving of the state
- Undo last move

# How to build

- Install [Alire](https://alire.ada.dev/)
- Build using `alr build`. All the dependencies are installed and managed by Alire.
- Run using `alr run` (mandatory in Windows so it find installed libraries) or
  directly from the working directory: `./play_2048` (Linux only).

# How to play
Use arrow keys to move tiles. When two adjacent tiles with the same number are merged, their numbers
add up. Keep merging tiles until you get to 2048. If the board is full and there is no possible move
(no adjacent tiles with the same number), you lose.

Keys:
- R - restart game
- Q - quit game
- Arrow keys - move tiles
- U - undo last move
- F11 - toggle fullscreen mode
- Tab - switch theme

# Attribution

The initial implementation of the game logic was taken from [Rosetta
Code](https://rosettacode.org/wiki/2048), although it has been mostly
rewritten.

Fonts used in the themes: Noto Serif (SIL Open Font License) for Theme
5 and DeJa Vu Sans (MIT License) for the rest.
