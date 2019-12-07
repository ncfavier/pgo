TARGET  = pgoc
ARCHIVE = favier
TEST   ?= -all

.PHONY: $(TARGET)
$(TARGET):
	ghc -isrc -outputdir build $(GHCFLAGS) -o $@ src/Main.hs

.PHONY: test
test:
	cd tests && ./test $(TEST) ../$(TARGET)

.PHONY: archive
archive:
	tar -cvzf $(ARCHIVE).tgz --transform 's,^,$(ARCHIVE)/,' src Makefile README.md

.PHONY: clean
clean:
	rm -rf build $(TARGET) $(ARCHIVE).tgz
