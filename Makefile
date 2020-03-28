all:
	ghc -Wall $(ARGS) plg-2-nka.hs -o plg-2-nka

clean:
	rm plg-2-nka *.hi *.o

pack:
	zip flp-fun-xlukac11.zip Makefile plg-2-nka.hs PLGParser.hs PLG2NKA_Data.hs README
