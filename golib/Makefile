GO ?= go
EMACS ?= emacs

memo.so: cmd/main.go
	$(GO) build -buildmode=c-shared -ldflags -s -o ../$@ $<

clean:
	rm -f  ../memo.so

debug:
	$(GO) build -o memo cmd/main.go
	chmod +x memo
	./memo --loglevel -1 daemon

run:
	./memo daemon

dev:
	$(GO) build -o memo cmd/main.go || exit
	chmod +x memo
	./memo --loglevel -1 daemon 

so: clean memo.so

check:
	make -C proto check

build:
	make -C gen run
	make -C proto gen

