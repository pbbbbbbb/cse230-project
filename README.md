# CSE230 Course Project

The course project for CSE230 Principles of Programming Language

## Application
We plan to design and implement a networked two-player aircraft shooting game with Haskell.

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

## Timeline
11.16 - 11.22	
* Get familiar with relevant libraries. 
* Finish gaming rules and logic design.
  
11.23 - 12.06
(12.01 checkpoint 2)
* Implement the user interface and gaming logic for a single user. 
* Design the corresponding server structure and server-client communication.
  
12.06 - 12.12
* Test and debug the game, including gaming logic and network communication.
  
12.13 - 12.15
* Finish the whole project
* Prepare for the demonstration

