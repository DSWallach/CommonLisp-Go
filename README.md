# CommonLisp-Go

A Go Game and A.I. implementation written for Allegro Common Lisp. 
This repository requires asdf for package management.
To install the required packages run the shell script install-deps.sh

To run the code, open up a lisp interperator in the src/ directory.

Run the commands (load "basic-defns") and (make)

(do-all-tests) will run the testing setup

(init-game) will return an instance of a go-game

Everything should now be freshly compiled and ready to use.

DATA

I have stored the networks I trained with this data as files so there is no need to
do the training again (it takes a while). However I thought I would include the data
none the less. 

9x9-Policy.tar.gz contains the data for the policy networks
9x9-Values.tar.gz contains the data for the value networks


TRAINED/EVOVLED NETWORKS (networks)

This contains the networks I've trained/evolved stored as text files. 
There are functions for reading from and writing this files defined in nn-go.lisp




SOURCE CODE (src)

Due to the number of functions the functionality of the go-game is 
broken up into several files

basic-defns.lisp  : Contains global constants, macros, and a few functions used for testing

go-game.lisp      : Contains go-game struct definition and functions related to it's state

groups.lisp       : Contains group struct definition and functions related to the state of an individual group

game-playing.lisp : Contains domain specific functions for go (e.g. legal-moves, do-move!, etc...)

testing.lisp      : Contains testing structure for validating the game playing setup. Some tests may fail for sizes other than 9x9


;;; Networks


nn-go.lisp        : This is mostly your provided neural networks with a few additional functions I implemented.

nn-operations     : This is mostly utility functions for performing operations on networks and running MCTS with networks.


