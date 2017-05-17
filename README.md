# CommonLisp-Go

A Go Game and A.I. implementation written for Allegro Common Lisp. 

To run the code, open up a lisp interperator in the src/ directory.

Run the commands (load "basic-defns") and (make)

(do-all-tests) will run the testing setup for the go-game and its functions

(init-game) will return an instance of a go-game

Everything should now be freshly compiled and ready to use.

DATA

I have stored the networks I trained with this data as files so there is no need to
do the training again (it takes a while). However I thought I would include the data
none the less. 

9x9-Policy.tar.gz contains the data for the policy networks
9x9-Values.tar.gz contains the data for the value networks


TRAINED/EVOLED NETWORKS (networks)

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



GENERAL COMMENTS

  By far the hardest and most time consuming part of this project was creating a go game
that will function correctly and give a decent estimate of the score at the end game.
Making a go game that was playable by humans was straight forward and pretty fast to play out.
Having to add checks for life and eyespace slowed it down significantly. 

  Some other issues that I didn't forsee was that changing the number of legal moves given 
for a particular board position caused index of bound errors when that board position was reached
after a different number of moves, either to to passing or a piece being captured.
Removing default good opening moves has decreased the A.I's ability but at this time I 
didn't see a better solution. I added an additional bit to the zobrist hashes to account for 
if there was a potential ko at a board position or not as that also will change the number of 
legal moves.

  I wasn't able to generate my own self-play data due to a lack of time. I set up an evolutionary 
algorithm to run it on junior without running into memory limitations. I seems like the networks are
too large to be able to have enough networks with enough threads running them to really use all of 
juniors cores. That being said I would recommend that you run your evaluations of my project on junior
as the threaded implementations will be significantly faster depending on the number of threads you set.
I did trained some value networks and started the addition of them into MCTS but I haven't had time to 
test it so I can't claim it as a working part of my project.

  There is an issues with running multiple threads of MCTS when using the random policy (no networks). I 
don't know if this has to do with how lisp handles the random state or what but I couldn't find a reason for
it in my code. An large amount of memory gets allocated for character arrays (according to (room t)).
This becomes a serious issue when running multiple threads as each thread will use over 1 GB.
This doesn't happen when running MCTS with the networks with or without multiple threads. 



RESULTS

  MCTS using one of the trained policy networks will beat regular MCTS when given the same parameters.
In particular the larger networks (full-*) seem to be better than the smaller one and the networks
trained with a higher learning rate will out perform the equivalent networks with a lower training 
rate.

TESTING

  The functions for testing are defined at the top of nn-operations.lisp. I didn't get everything 
accomplished but I hope this is at least an interesting project.
