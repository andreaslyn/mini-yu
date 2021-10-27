ROOT_DIR := $(shell for i in `seq 0 9`; \
  do if [ -n "`ls | grep Makefile.inc`" ]; \
  then pwd; break; else cd ..; fi; done)

CFLAGS := -fyu-stack -fno-omit-frame-pointer
CC := $(ROOT_DIR)/../gcc/yu-stack-install/bin/gcc

include $(ROOT_DIR)/Makefile.inc
