all: sendfile sendfile.js

sendfile.js: sendfile.hs
	hastec sendfile.hs

sendfile:
	ghc --make sendfile.hs

clean:
	-rm -r main
	-rm *~
	-rm sendfile.hi
	-rm sendfile.o

distclean: clean
	-rm sendfile
	-rm sendfile.js
