all:
	ghc -o poker Main.hs

clean:
	rm -r *.o *.hi poker
