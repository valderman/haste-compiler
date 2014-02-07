all: chatbox chatbox.js

chatbox.js: chatbox.hs
	hastec chatbox.hs

chatbox:
	ghc --make chatbox.hs

clean:
	-rm -r main
	-rm *~
	-rm chatbox.hi
	-rm chatbox.o

distclean: clean
	-rm chatbox
	-rm chatbox.js
