# NineMorris Client Makefile
# fallback for non-cabal users
# better use the cabal buildfile

CC = ghc
CFLAGS = -Wall -fno-warn-unused-do-bind -threaded -O2 -auto-all -debug -rtsopts -with-rtsopts -N
SOURCES = ./NineMorris/main.hs
BUILDDIR = dist
APPNAME = client

all: debug

debug:
	mkdir -p $(BUILDDIR)
	$(CC) $(CFLAGS) -outputdir $(BUILDDIR) -o $(BUILDDIR)/$(APPNAME) $(SOURCES)

clean:
	rm -r ./$(BUILDDIR)/*
