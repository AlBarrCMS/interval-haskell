all: rin main boxes

boxes:
	ghc -O3 RIN_box_num.hs -o rin_box_num

rin:
	ghc -O3 TestRIN.hs -o rin

main:
	ghc -O3 Main.hs

prof:
	ghc -prof -fprof-auto -rtsopts Main.hs

proftest: prof
	./Main +RTS -p

test: main
	./Main && xdg-open test.ppm

clean:
	rm *.o *.hi Main rin rin_box_num

