PROJECT_PATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
INSTALL_PATH := $(PROJECT_PATH)/bin
EXE_PATH := $(INSTALL_PATH)/mini-yu-exe

MAKEFLAGS := --no-print-directory $(MAKEFLAGS)

LIBMIMALLOC := mimalloc/out/libmimalloc.a

ifeq ($(DEBUG), 1)
	FAST := --fast
endif

ifeq ($(PEDANTIC), 0)
	PEDANT :=
else
	PEDANT := --pedantic
endif

ifeq ($(PROFILE), 1)
	PROF := --profile
else
	PROF :=
endif

.PHONY: mini-yu
mini-yu: $(EXE_PATH)

.PHONY: config
config:
	mkdir -p mimalloc/out
	cmake -Hmimalloc -Bmimalloc/out -DMI_OVERRIDE=OFF

all: config mini-yu

.PHONY: $(EXE_PATH)
$(EXE_PATH): $(LIBMIMALLOC) libyur
	@PATH="$(PATH):$(INSTALL_PATH)" stack install $(PROF) $(PEDANT) $(FAST) --local-bin-path "$(INSTALL_PATH)"

.PHONY: $(LIBMIMALLOC)
$(LIBMIMALLOC):
	$(MAKE) -C mimalloc/out

.PHONY: libyur
libyur:
	@$(MAKE) -C runtime

.PHONY: base_clean
base_clean:
	rm -rf $(EXE_PATH)

clean: base_clean
	stack clean

cleanall: base_clean
	stack purge
