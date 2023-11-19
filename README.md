[![Build](https://github.com/mgrojo/play_2048/actions/workflows/main.yml/badge.svg)](https://github.com/mgrojo/play_2048/actions/workflows/main.yml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/play_2048.json)](https://alire.ada.dev/crates/play_2048.html)
[![Alire CI/CD](https://img.shields.io/endpoint?url=https://alire-crate-ci.ada.dev/badges/play_2048.json)](https://alire-crate-ci.ada.dev/crates/play_2048.html)
[![Download][download-img]][download]
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/ada-lang/Lobby)
[![Mentioned in Awesome Ada](https://awesome.re/mentioned-badge.svg)](https://github.com/ohenley/awesome-ada)
![Screenshot of Play 2048!](/images/screenshot_linux_4x4.png)


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
- Three board sizes:
  - Small (3x3): difficult, beat your score!
  - Default (4x4): normal, you can do it!
  - Easy (5x5): easy, beat your best time!

# How to build

- Install [Alire](https://alire.ada.dev/)
- Build using `alr build`. All the dependencies are installed and managed by Alire.
- Run using `alr run` (mandatory in Windows so it finds installed libraries) or
  directly from the working directory: `./play_2048` (Linux only).

# How to install

The game can be installed using Alire 2.0 with this command:
```
alr install
```
Refer to Alire documentation on how to add the Alire default installation to the PATH variable
or how  to change the default installation prefix.

If Alire 2.0 is not available, you can still install the application using something like this:
```
sudo make PREFIX=/opt/play_2048 install
```

If you're interested in building the AppImage file:
```
make AppImage
```

# How to play

Use arrow keys to move tiles. When two adjacent tiles with the same number are merged, their numbers
add up. Keep merging tiles until you get to 2048. If the board is full and there is no possible move
(no adjacent tiles with the same number), you lose.

To change the board size, export the enviroment variable
PLAY_2048_SIZE to the number of cells per size, for example, in
Linux/Bash:

```bash
export PLAY_2048_SIZE=3
```
After a change in the board size, the board state will not be restored.

Keys:
- <kbd>R</kbd> - restart game
- <kbd>Q</kbd> - quit game
- Arrow keys - move tiles
- <kbd>U</kbd> - undo last move
- <kbd>F11</kbd> - toggle fullscreen mode
- <kbd>Tab</kbd> - switch theme

# Attribution

The initial implementation of the game logic was taken from [Rosetta
Code](https://rosettacode.org/wiki/2048), although it has been mostly
rewritten.

Fonts used in the themes: Noto Serif (SIL Open Font License) for Theme
5 and DeJa Vu Sans (MIT License) for the rest.


  [download-img]: https://img.shields.io/github/downloads/mgrojo/play_2048/total.svg
  [download]: https://github.com/mgrojo/play_2048/releases
