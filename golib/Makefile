GO ?= go
EMACS ?= emacs

.PTHON: gen

libmemo.so: cmd/libmemo/main.go
	$(GO) build -buildmode=c-shared -ldflags -s -o ../$@ $<

clean:
	rm -f  ../libmemo.so

gen:
	make -C gen run

so: clean gen libmemo.so
