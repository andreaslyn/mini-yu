ifneq ($(VERBOSE), y)
	MAKEFLAGS := --no-print-directory $(MAKEFLAGS)
endif

build:
ifneq ($(VERBOSE), y)
	@$(MAKE) -C split-stack
	@$(MAKE) -C system-stack
else
	$(MAKE) -C split-stack
	$(MAKE) -C system-stack
endif

clean:
ifneq ($(VERBOSE), y)
	@$(MAKE) -C split-stack clean
	@$(MAKE) -C system-stack clean
else
	$(MAKE) -C split-stack clean
	$(MAKE) -C system-stack clean
endif
