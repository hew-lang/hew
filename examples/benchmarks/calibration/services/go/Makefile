OUT ?= ../out
GOCACHE ?= $(OUT)/go-cache

.PHONY: build run

build:
	mkdir -p "$(OUT)"
	GOCACHE="$(GOCACHE)" go build -buildvcs=false -o "$(OUT)/service-go" .

run: build
	"$(OUT)/service-go"
