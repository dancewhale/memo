GO ?= go
EMACS ?= emacs

.PTHON: gen

server: cmd/main.go
	$(GO) build -o ../$@ $<

clean:
	rm -f  ../server

gen:
	make -C gen run

tidy:
	$(GO) mod tidy

build: clean gen tidy server
