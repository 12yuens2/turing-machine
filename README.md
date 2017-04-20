# Turing Machine

## Running the machine
To build the executable, run the following commands:	
- `cabal configure` to configure if needed.
- `cabal build` to build the executable.

The executable is called `runtm` and can be found in dist/build/runtm/

runtm takes two files as arguments: A description of a Turing Machine and an text input file.

eg, `runtm TMs/palindrome.tm input.txt`

The description must be in the form specified in TuringMarchineParser.hs. Refer to the documentation found at docs/ or https://studres.cs.st-andrews.ac.uk/CS3052/Practicals/P01-turingmachines.pdf for more details.

The input text file will be taken literally, character by character and written into the Tape of the Turing Machine. If there are any characters in the Tape that are not part of the alphabet in the Turing Machine description, the machine will fail.

A few example TMs can be found in TMs/
