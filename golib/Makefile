GO ?= go
EMACS ?= emacs

memo.so: cmd/main.go
	$(GO) build -buildmode=c-shared -ldflags -s -o $@ $<

clean:
	rm -f  memo.so

all: memo.so
