tests: TestFastSet.out

%.out: %.hs
	runghc $< > $<.tmp
	mv $<.tmp $@
