# CSE230 Course Project

The course project for CSE230 Principles of Programming Language

## Application
We plan to design and implement a two-player aircraft shooting game with Haskell.

## Goals
The major tasks in implementing the game includes:
* Implement a terminal user interface (TUI) with [`brick`](https://hackage.haskell.org/package/brick) library
  * Use the declarative APIs to describe our gaming interface.
  * Use the event handlers to handle different gaming events, e.g., player’s movement, shooting, and maintaining player’s life/score.
* Design a set of gaming rules for players including:
  * Players can move their aircraft using keyboard ‘W’, ‘A’, ‘S’, ‘D’.
  * Players' aircrafts automatically shoot out bullets towards the upper part of the screen at a constant rate.
  * Players get credit when they hit the enemy's aircrafts and destroy them.
  * Players' life reduces when crashing into the enemy's aircrafts or hit by the bullets from the enemy's aircrafts.
  * The game is over when a player’s life decreases to zero.
  * Two players can each control one aircraft during a game. At the end of the game, the player who got higher credit wins.
* Design a set of gaming logic for the enemy including:
  * Enemy's aircrafts emerge randomly from the upper screen.
  * The enemy's aircrafts can attack players in the following way:
    * Directly smash into the player.
    * Shoot out bullets around.
    * Shoot out a line of laser which can be regarded as a whole line of bullets.
* Implement inter-player communication with haskell [`Network`](https://wiki.haskell.org/Applications_and_libraries/Network) library
  * We achieve the two-player mode in a client-server manner, which allows potentially increase the number of players involved.
  * The players runs in the client side with the user interfaces and part of the gaming logic code.
  * The server maintains data structure that stores important information and is responsible for synchronising shared data among users.

## Project Architecture

* `UI.sh` - implemented with bricks
  * Keyboard
    * ‘W’, ‘A’, ‘S’, ‘D’ - move the player
    * ‘P’ - pause
    * ‘Esc’ - exit
* `Player.sh` - game objects and relevant operations
  * Player - move, shoot bullet, check health…
    * Data: score, coordination, health
    * Related functions: getScore, setScore, isAlive, move, etc.
  * Enemy - move, shoot bullet
    * Data: score, coordination, health, fire rate
    * Related functions: getScore, setScore, isAlive, move, shootBullet etc.
  * Player bullet
  * Enemy bullet
* `Game.sh` - game frame
  * Data: player, enemy, bullets
  * Game status: game over,  pause, timer
  * `Tick` function
    * Increment timer
    * Update player: check crash, move, generate bullet
    * Update enemy: check crash, move, generate bullet
    * Update bullet: move
    * Return `IO Game` to UI

## How to play

### Prerequisites

Before you begin, ensure you have the following installed:

* [Stack](hhttps://docs.haskellstack.org/en/stable/)

### Build

To build the game, run the following command:

```bash
$ stack build
```

### Run

To start the game, run:

```bash
$ stack run
```

## Unit Test

Our game includes a robust set of unit tests, housed in the "UnitTest" module. These tests help ensure the game's reliability and performance.

Run the following command to run the unit test.

```bash
$ stack test
```

## Task Allocation

* Zhangchi Lu: Property and behavior of Player, Enemy and Bullets
* Yangkun Wang: Main UI
* Bei Pei: Game events

