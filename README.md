# CommonLisp-Go

A Go Game and A.I. implementation written for Allegro Common Lisp. 
This repository requires asdf for package management.
To install the required packages run the shell script install-deps.sh

To run the code, open up a lisp interperator in the src/ directory.

Run the commands (load "basic-defns") and (make)

(do-all-tests) will run the testing setup

(init-game) will return an instance of a go-game

Everything should now be freshly compiled and ready to use.

SOURCE

Due to the number of functions the functionality of the go-game is 
broken up into several files

basic-defns.lisp  : Contains global constants, macros, and a few functions used for testing

go-game.lisp      : Contains go-game struct definition and functions related to it's state

groups.lisp       : Contains group struct definition and functions related to the state of an individual group

game-playing.lisp : Contains domain specific functions for go (e.g. legal-moves, do-move!, etc...)

testing.lisp      : Contains testing structure for validating the game playing setup. Some tests will fail for sizes other than 9x9



