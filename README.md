This is another implementation of the [2048 game](https://github.com/gabrielecirulli/2048)
implemented in Ada using [ASFML](https://mgrojo.github.io/ASFML/).

# How to build

- Install [Alire](https://alire.ada.dev/)
- Build using `alr build`. All the dependencies are installed and managed by Alire.
- Run directly from the working directory: `./play_2048`

# How to play
Use arrow keys to move tiles. When two adjacent tiles with the same number are merged, their numbers
add up. Keep merging tiles until you get to 2048. If the board is full and there is no possible move
(no adjacent tiles with the same number), you lose.

Keys:
- R - restart game
- Q - quit game
- Arrow keys - move tiles
- F11 - toggle fullscreen mode

# Attribution

The initial implementation of the game logic was taken from [Rosetta Code](https://rosettacode.org/wiki/2048).
