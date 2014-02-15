RUSTC ?= rustc
RUSTFLAGS ?= -A unused-variable -A dead-code -O
RUST = $(RUSTC) $(RUSTFLAGS)

SRC = src
BUILD = build
LIBDIR = lib

TARGET = $(BUILD)/rustboy
DEPS = $(BUILD)/.depends

.PHONY: all clean

all: $(TARGET)

clean:
	rm -f $(TARGET) $(DEPS)
	rm -d $(BUILD)

$(TARGET): $(SRC)/main.rs |$(BUILD)
	$(RUST) $< -L $(LIBDIR) -o $@ --dep-info $(DEPS)

$(BUILD):
	mkdir $(BUILD)

-include $(DEPS)
