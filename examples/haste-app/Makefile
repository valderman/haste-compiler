all: haste-app haste-app.js

haste-app.js: haste-app.hs
	hastec haste-app.hs

haste-app:
	ghc --make haste-app.hs

clean:
	-rm -r main
	-rm *~
	-rm haste-app.hi
	-rm haste-app.o

distclean: clean
	-rm haste-app
	-rm haste-app.js
