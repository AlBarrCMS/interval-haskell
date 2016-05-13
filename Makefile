all:  test

main:
	ghc Main.hs

prof:
	ghc -prof -fprof-auto -rtsopts Main.hs

proftest: prof
	rm test.ppm && ./Main +RTS -p


test: main
	rm test.ppm && ./Main && xdg-open test.ppm		

clean:
	rm *.o Main 
    
