GO ?= go
EMACS ?= emacs

memo.so: cmd/main.go
	$(GO) build -buildmode=c-shared -ldflags -s -o $@ $<

clean:
	rm -f  memo.so

run:
	$(GO) build -o memo cmd/main.go
	./memo -c ./config.yaml

so: memo.so
