target   = pgoc
archive  = favier
test    ?= -all

.PHONY: $(target)
$(target):
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ --make src/Main.hs

.PHONY: test
test:
	cd tests && ./test $(test) ../$(target)

.PHONY: archive
archive:
	tar -cvzf $(archive).tgz --transform 's,^,$(archive)/,' src Makefile LISEZMOI.md pgoc

.PHONY: clean
clean:
	rm -rf build $(target) $(archive).tgz
