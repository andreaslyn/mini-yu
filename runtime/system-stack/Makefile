ROOT_DIR := $(shell for i in `seq 0 9`; \
  do if [ -n "`ls | grep Makefile.inc`" ]; \
  then pwd; break; else cd ..; fi; done)

CFLAGS := -Dyur_DISABLE_SPLIT_STACK
CC := gcc

include $(ROOT_DIR)/Makefile.inc
