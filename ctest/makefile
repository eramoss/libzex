CC = zig cc
OUT_DIR = ../zig-out/cbin

SRC_DIR = .

SRC_FILES = $(wildcard $(SRC_DIR)/*.c)
INCLUDE_PATH = ../include
LIB_PATH = ../zig-out/lib
LIB_NAME = -l:libzex.a

CFLAGS = -Wall -Wextra -g

all: $(patsubst $(SRC_DIR)/%.c, $(OUT_DIR)/%, $(SRC_FILES))

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

$(OUT_DIR)/%: $(SRC_DIR)/%.c | $(OUT_DIR)
	$(CC) -o $@ $< -I$(INCLUDE_PATH) -L$(LIB_PATH) $(LIB_NAME) $(CFLAGS)

clean:
	rm -f $(OUT_DIR)/*

.PHONY: all clean
