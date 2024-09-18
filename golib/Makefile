GO ?= go
EMACS ?= emacs

libmemo.so: cmd/main.go
	$(GO) build -buildmode=c-shared -ldflags -s -o ../$@ $<

clean:
	rm -f  ../memo.so

debug:
	$(GO) build -o memo cmd/main.go || exit
	chmod +x memo
	./memo --loglevel -1 daemon

run:
	./memo daemon

build:
	$(GO) build -o memo cmd/main.go || exit

so: clean libmemo.so

check:
	make -C proto check

gen:
	make -C gen run
	make -C proto gen

