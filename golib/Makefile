GO ?= go
EMACS ?= emacs

memo.so: cmd/main.go
	$(GO) build -buildmode=c-shared -ldflags -s -o $@ $<

clean:
	rm -f  memo.so

run:
	$(GO) build -o memo cmd/main.go
	chmod +x memo
	./memo -c ./config.yaml

so: clean memo.so

check:
	make -C proto check

build:
	make -C gen run
	make -C proto gen

