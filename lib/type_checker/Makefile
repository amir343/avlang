SRC = src
EBIN = ebin

MODULES =  \
	state_dl \
	terl_any \
	terl_boolean \
	terl_binary \
	terl_float \
	terl_integer \
	terl_list \
	terl_pid \
	terl_string \
	terl_union \
	type_check \
	type_check_compiler_opts \
	type_err_msg \
	type_internal

HRL_FILES= \
	type_macros.hrl \
	type_checker_state.hrl

ERLANG_TYPES = priv/erlang_types.eterm

ERL_FILES= $(MODULES:%=$(SRC)/%.erl)
HEADER_FILES = $(HRL_FILES:%=$(SRC)/%)
BEAM_FILES = $(MODULES:%=$(EBIN)/%.beam)

INSTALL_FILES= $(BEAM_FILES) $(ERLANG_TYPES)
TARGET_FILES= $(INSTALL_FILES)


# ------------------------------------------------------------------------------
# Flags
# ------------------------------------------------------------------------------

ERL_COMPILE_FLAGS += +inline +warn_unused_import -Werror -W

# ------------------------------------------------------------------------------
# Targets
# ------------------------------------------------------------------------------

$(EBIN)/%.beam: $(SRC)/%.erl
	erlc $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

compile: $(TARGET_FILES)

clean:
	rm -fr $(BEAM_FILES)

