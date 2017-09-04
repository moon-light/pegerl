# Configuration
PEG_PATH ?= src/

# Verbosity.
peg_verbose_0 = @echo " PEG   " $(?F);
peg_verbose = $(peg_verbose_$(V))

# Core targets.
PEG_FILES = $(sort $(call core_find,$(PEG_PATH),*.peg))

# Don't run if no peg file
ifneq ($(PEG_FILES),)
# Rebuild peg files when the Makefile changes.
$(PEG_FILES): $(MAKEFILE_LIST)
	@touch $@

define pegerl_compile.erl
	[ok = pegerl:generate(F) || F <- string:tokens("$(1)", " ")],
	halt().
endef

# Generate .erl file from .peg before compile .erl happened
$(PROJECT).d:: $(PEG_FILES)
	$(peg_verbose) $(call erlang,$(call pegerl_compile.erl,$<))
endif
