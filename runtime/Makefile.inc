# Command line flags:
#   DBG=y -> Don't include debugging information. (Default is DBG=n).
#     Note that DBG has nothing to do with the CPP NDEBUG macro.
#   VERBOSE=y -> Print verbose messages. (By default VERBOSE=n).
#   OPT=n -> Don't optimize and define the NDEBUG CPP macro. (Default OPT=y).

ifndef MAKEFILE_INC
MAKEFILE_INC := y

# CFLAGS =
# DEPDIRS =
# CC =

CSOURCE := \
  $(ROOT_DIR)/yucmalloc.c \
  $(ROOT_DIR)/yustr.c \
  $(ROOT_DIR)/parallel.c \
  $(ROOT_DIR)/yu.c \
  $(ROOT_DIR)/yulist.c \
  $(ROOT_DIR)/yustack.c
SSOURCE := \
  $(ROOT_DIR)/yustack.S

SUB_DIR_SHELL := $(shell \
  PWD_LEN_INC="`expr length $$(pwd)`"; \
  ROOT_DIR_LEN_INC=`expr length $(ROOT_DIR)/`; \
  expr substr `pwd` `expr $$ROOT_DIR_LEN_INC + 1` \
    `expr $$PWD_LEN_INC - $$ROOT_DIR_LEN_INC`)

OUT_DIR := $(ROOT_DIR)/out/$(SUB_DIR_SHELL)

WORKING_DIR := $(shell pwd)

LIBYUR := $(OUT_DIR)/libyur.a

ifeq ($(DBG), y)
	DBG_STR := -g
else
	DBG_STR :=
endif

override CFLAGS += -MMD -std=gnu11 -c -Wall $(DBG_STR) \
	-I$(ROOT_DIR)/../mimalloc/include

ifneq ($(OPT), n)
	override OPT := y
endif
ifeq ($(OPT), y)
	override CFLAGS += -O3 -DNDEBUG -momit-leaf-frame-pointer
endif

COBJ := $(CSOURCE:$(ROOT_DIR)/%.c=$(OUT_DIR)/%.c.o)
CDEP := $(CSOURCE:$(ROOT_DIR)/%.c=$(OUT_DIR)/%.c.d)
SOBJ := $(SSOURCE:$(ROOT_DIR)/%.S=$(OUT_DIR)/%.S.o)
SDEP := $(SSOURCE:$(ROOT_DIR)/%.S=$(OUT_DIR)/%.S.d)
OBJ := $(COBJ) $(SOBJ)
DEP := $(CDEP) $(SDEP)

build: outdir.inc depdirs.inc $(LIBYUR)

outdir.inc:
	@mkdir -p $(OUT_DIR)

depdirs.inc:
	@for d in $(DEPDIRS); do \
	  $(MAKE) -C $$d || exit $$?; \
	done

$(LIBYUR): $(OBJ)
	ar rc $(LIBYUR) $^

$(OBJ):
ifeq ($(VERBOSE), y)
	$(CC) $(CFLAGS) -o $@ $(@:$(OUT_DIR)/%.o=$(ROOT_DIR)/%)
else
	@echo CC $(@:%.o=%.c)
	@$(CC) $(CFLAGS) -o $@ $(@:$(OUT_DIR)/%.o=$(ROOT_DIR)/%)
endif
-include $(DEP)

clean: depdirs_clean.inc clean_cobj.inc clean_cdep.inc clean_libyur.inc

depdirs_clean.inc:
	@for d in $(DEPDIRS); do \
	  $(MAKE) -C $$d clean || exit $$?; \
	done

clean_cobj.inc:
ifeq ($(VERBOSE), y)
	@for f in $(OBJ); do \
	  echo $(RM) $$f; \
	  $(RM) $$f; \
	done
else
ifdef CSOURCE
	@echo clean $(OUT_DIR)
	@for f in $(OBJ); do \
	  $(RM) $$f; \
	done
endif
endif

clean_cdep.inc:
ifeq ($(VERBOSE), y)
	@for f in $(DEP); do \
	  echo $(RM) $$f; \
	  $(RM) $$f; \
	done
else
	@for f in $(DEP); do \
	  $(RM) $$f; \
	done
endif

clean_libyur.inc:
ifeq ($(VERBOSE), y)
	$(RM) $(LIBYUR)
else
	@$(RM) $(LIBYUR)
endif

endif # MAKEFILE_INC
# vim:ft=make
