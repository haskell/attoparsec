all: TestFastSet.out QC.out

%.out: %.exe
	./$< | tee $<.tmp
	mv $<.tmp $@

%.exe: %.hs
	ghc -O -fno-warn-orphans --make -o $@ $<

clean:
	-rm -f *.hi *.o *.exe *.out
