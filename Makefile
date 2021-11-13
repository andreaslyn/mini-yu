PROJECT_PATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
INSTALL_PATH := $(PROJECT_PATH)/bin
EXE_PATH := $(INSTALL_PATH)/mini-yu-exe

MAKEFLAGS := --no-print-directory $(MAKEFLAGS)

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
mini-yu: $(EXE_PATH) mini-yu-stdlib

.PHONY: config
config:
	mkdir -p mimalloc/out
	cmake -Hmimalloc -Bmimalloc/out -DMI_OVERRIDE=OFF

all: config custom-gcc mini-yu

.PHONY: $(EXE_PATH)
$(EXE_PATH): make-split-stack-header libmimalloc libyur
	@PATH="$(PATH):$(INSTALL_PATH)" stack install $(PROF) $(PEDANT) $(FAST) --local-bin-path "$(INSTALL_PATH)"

.PHONY: make-split-stack-header
make-split-stack-header:
	@echo "-- Auto generated. Do not edit." > app/split-stack-config.h
ifeq ($(SPLITSTACK), 0)
	echo "#define NO_SPLIT_STACK True" >> app/split-stack-config.h
else
	@if [ ! -x gcc/yu-stack-install/bin/gcc ]; then \
		echo Split stack support requires building a custom gcc. Use:; \
		echo "\t\"$(MAKE) custom-gcc\" to build it (will take a long time), or use"; \
		echo "\t\"$(MAKE) SPLITSTACK=0\" to disable split stack support"; \
		exit 1; \
	fi
	echo "#define NO_SPLIT_STACK False" >> app/split-stack-config.h
endif

.PHONY: libmimalloc
libmimalloc:
	$(MAKE) -C mimalloc/out

.PHONY: libyur
libyur:
	@$(MAKE) -C runtime

.PHONY: custom-gcc
custom-gcc:
	if [ ! -d gcc ]; then ./configure-gcc.sh; fi
	$(MAKE) -C gcc/yu-stack-build
	$(MAKE) -C gcc/yu-stack-build install

.PHONY: base_clean
base_clean:
	rm -rf $(EXE_PATH)

.PHONY: mini-yu-stdlib
mini-yu-stdlib:
	$(PROJECT_PATH)/yuc $(PROJECT_PATH)/stdlib/yu/main.yu -co

clean: base_clean
	stack clean
	rm -r $(PROJECT_PATH)/stdlib/yu/.yupackage

cleanall: base_clean
	stack purge
	rm -r $(PROJECT_PATH)/stdlib/yu/.yupackage
