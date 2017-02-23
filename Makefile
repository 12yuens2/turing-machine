HSC = ghc

all: turing

turing: turing.hs
	ghc turing.hs -o runtm

clean:
	rm -f runtm
