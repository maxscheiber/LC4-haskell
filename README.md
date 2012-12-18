LC4-haskell
===========

Purely functional interpreter for the educational LC-4 ISA, written by Max Scheiber (scheiber), and Ashutosh Goel (asgoel).

Main
===========

To compile, run ghc --make Main.hs. This will compile Main.hs and all of its dependencies automatically. To run, type ./Main. This will open up a repl in which you can input the assembly filename, and the program will assemble and process the code, and print out the final Machine state when done. Note that this repl is very basic and reads each file one at a time. One idea for future work is to allow for multiple file execution (i.e. if one file needs another file as well). 

Parser
=============

The Parser library is a generic monadic Parser, built off of Dr. Stephanie Weirich's version for CIS 552 at the University of Pennslyvania.

LC4
============

The LC4 library is a collection of files which defines the typesystem used in the LC4 simulator, as well as a custom Parser for assembly files built off of the Parser library mentioned above. The LC4 contains all the code neccessary for interpreting instructions, including preprocessing and executing an assembly file. The key component, the machine state, is built off of the State monad and is kept and updated throughout execution. The LC4 library also contains basic error checking, based off of
common errors seen in LC4 assembly code.


