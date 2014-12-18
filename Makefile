# NineMorris Client Makefile
# fallback for non-cabal users
# better use the cabal buildfile

CC = ghc
CFLAGS = -O2
SOURCES = ./NineMorris/main.hs
BUILDDIR = dist
APPNAME = client

all: debug

debug:
	mkdir -p $(BUILDDIR)
	$(CC) $(CFLAGS) -outputdir $(BUILDDIR) -o $(BUILDDIR)/$(APPNAME) $(SOURCES)

clean:
	rm -r ./$(BUILDDIR)/*
