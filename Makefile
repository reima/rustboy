RUSTC ?= rustc
RUSTFLAGS ?= -A unused-variable -A dead-code
RUST = $(RUSTC) $(RUSTFLAGS)

SRC = src
BUILD = build

TARGET = $(BUILD)/rustboy
DEPS = $(BUILD)/.depends

.PHONY: all clean

all: $(TARGET)

clean:
	rm -f $(TARGET) $(DEPS)
	rm -d $(BUILD)

$(TARGET): $(SRC)/main.rs
	mkdir -p $(BUILD)
	$(RUST) $< -o $@ --dep-info $(DEPS)

-include $(DEPS)
