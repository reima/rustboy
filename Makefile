RUSTC ?= rustc
RUSTFLAGS ?= -A unused-variable -A dead-code
RUST = $(RUSTC) $(RUSTFLAGS)

SRC = src
BUILD = build

TARGET = $(BUILD)/rustboy
DEPS = $(BUILD)/.depends

.PHONY: all

all: $(TARGET)

$(TARGET): $(SRC)/main.rs
	$(RUST) $< -o $@ --dep-info $(DEPS)

-include $(DEPS)
