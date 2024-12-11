.PHONY: all build run format clean

WISHES_FILE := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS)) # Some witchcraft

EXE_NAME?=wish_granter
WISHES_DIR?=wishes

all: build

build:
	@echo "\n   🚨  COMPILING  🚨"
	@dune build src/$(EXE_NAME).exe
	@ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

run: build
	@echo "\n   ⚡   EXECUTING   ⚡\n"
	@[ `echo -n $(WISHES_FILE) | wc -c` -gt 0 ] || (echo "Please provide a wishes file name" && exit 1)
	@[ -f $(WISHES_DIR)/$(WISHES_FILE) ] || (echo "File $(WISHES_DIR)/$(WISHES_FILE) does not exist" && exit 1)
	@./$(EXE_NAME).exe $(WISHES_DIR)/$(WISHES_FILE)

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	rm -f *.dot
	dune clean

%::
	@true # To complete the spell
