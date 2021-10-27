ifneq ($(VERBOSE), 1)
	MAKEFLAGS := --no-print-directory $(MAKEFLAGS)
endif

build:
ifneq ($(VERBOSE), 1)
ifneq ($(SPLITSTACK), 0)
	@$(MAKE) -C split-stack
endif
	@$(MAKE) -C system-stack
else
ifneq ($(SPLITSTACK), 0)
	$(MAKE) -C split-stack
endif
	$(MAKE) -C system-stack
endif

clean:
ifneq ($(VERBOSE), 1)
	@$(MAKE) -C split-stack clean
	@$(MAKE) -C system-stack clean
else
	$(MAKE) -C split-stack clean
	$(MAKE) -C system-stack clean
endif
