all: TestFastSet.out qc.out

%.out: %.exe
	./$< | tee $<.tmp
	mv $<.tmp $@

qc.exe: QC.hs
	ghc -O -fno-warn-orphans --make -o $@ $<

%.exe: %.hs
	ghc -O -fno-warn-orphans --make -o $@ $<

clean:
	-rm -f *.hi *.o *.exe *.out
