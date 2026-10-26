HEW ?= ../../../../../build/bin/hew
OUT ?= $(CURDIR)/out

.PHONY: hew driver go rust dotnet beam
hew:
	mkdir -p "$(OUT)"
	"$(HEW)" build --release service.hew -o "$(OUT)/service-hew"
driver:
	mkdir -p "$(OUT)"
	go build -o "$(OUT)/service-driver" driver/main.go
go rust dotnet beam:
	$(MAKE) -C $@ OUT="$(OUT)" build
