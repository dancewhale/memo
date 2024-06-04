BUF ?= buf

gen:
	$(BUF) generate

check:
	$(BUF) lint
	$(BUF) breaking --against "../../.git#branch=main,subdir=golib/proto"
