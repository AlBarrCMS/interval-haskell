all:  main

main:
	ghc -O3 Main.hs

prof:
	ghc -prof -fprof-auto -rtsopts Main.hs

proftest: prof
	./Main +RTS -p


test: main
	./Main && xdg-open test.ppm		

clean:
	rm *.o *.hi Main 
    
